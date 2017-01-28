module Bot (main, run) where

import Bot.Args
import Bot.Utils
import Control.Concurrent.VarThread
import Sources.Gosu
import Sources.PrizePool
import qualified Sources.Azubu as Azubu
import qualified Sources.Gosu as Gosu
import qualified Sources.Hitbox as Hitbox
import qualified Sources.MLG as MLG
import qualified Sources.Twitch as Twitch

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.List
import Data.Map (Map)
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Yaml
import Prelude
import Reddit hiding (Options)
import Reddit.Types.Wiki
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Googl

data Stream = TStream Twitch.Stream
            | MStream (MLG.Stream, MLG.Channel)
            | AStream Azubu.Stream
            | HStream Hitbox.Stream
  deriving (Show, Read, Eq)

streamViewers :: Stream -> Integer
streamViewers (TStream s) = Twitch.viewers s
streamViewers (MStream (s, _)) = MLG.streamViewers s
streamViewers (AStream s) = Azubu.viewers s
streamViewers (HStream s) = Hitbox.viewers s

type URLMap = Map Text Text

type Whitelist = Set Text

main :: IO ()
main = getOpts >>= run

run :: CmdOptions -> IO ()
run (CmdOptions fp) =
  decodeFileEither fp >>= \case
    Left err -> do
      print err
      exitFailure
    Right x -> redesign x

redesign :: Options -> IO ()
redesign o@(Options (Username u) p r gg wk gk) = do
  pp <- prizeTrackerThread wk
  wp <- newEmptyVarThread (wikiPageTracker o)
  whitelist <- newEmptyVarThread (whitelistTracker o)
  ts <- Twitch.twitchTrackerThread
  as <- Azubu.azubuTrackerThread
  hs <- Hitbox.hitboxTrackerThread
  mlgs <- MLG.mlgTrackerThread
  urlmap <- newTVarIO Map.empty
  mp <- Gosu.gosuTrackerThread gg
  let redditOpts = defaultRedditOptions { loginMethod = Credentials u p
                                        , customUserAgent = Just "/r/Dota2 sidebar bot" }
  forever $ do
    (prize, wikiText, streams, ms) <- atomically $
      (,,,) <$> readVarThread pp
            <*> readVarThread wp
            <*> combine whitelist ts mlgs as hs
            <*> readVarThread mp
    void $ runRedditWith redditOpts $ do
      currentTime <- liftIO getCurrentTime
      shorteneds <- mapM (liftIO . retrieveShortened gk urlmap . Gosu.matchURL) ms
      let (days, hours, minutes) = countdownDiff currentTime monkeyKingReleaseDate
      app <- return $ mconcat $ map Endo
        [ Text.replace "%%PRIZE%%" $ "$" <> thousandsFormat prize
        , Text.replace "%%STREAMS%%" $ formatStreams $ take 5 $ sortBy (comparing (Down . streamViewers)) streams
        , Text.replace "%%MATCHES%%" $ formatMatches currentTime $ ms `zip` shorteneds
        , Text.replace "%%ANNOUNCEMENTS%%" ""
        , Text.replace "%%COUNTDOWN_DAYS%%" $ tshow days
        , Text.replace "%%COUNTDOWN_HOURS%%" $ tshow hours
        , Text.replace "%%COUNTDOWN_MINUTES%%" $ tshow minutes ]
      editWikiPage r "config/sidebar" (appEndo app wikiText) "sidebar update"
    threadDelay $ 60 * 1000 * 1000

countdownDiff :: UTCTime -> UTCTime -> (Integer, Integer, Integer)
countdownDiff now target = (diffDays (utctDay target) (utctDay now), hours, minutes)
  where
    seconds = ceiling $ utctDayTime target - utctDayTime now
    minutes = (seconds `div` 60) `mod` 60
    hours = (minutes `div` 60) `mod` 24

monkeyKingReleaseDate :: UTCTime
monkeyKingReleaseDate = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2016-12-12 15:22:00 PST"

allowed :: Whitelist -> [Twitch.Stream] -> [Twitch.Stream]
allowed wl = filter (\s -> Twitch.broadcasterLanguage s == "en" || Set.member (Text.toLower $ Twitch.streamer s) wl)

combine :: VarThread Whitelist -> VarThread [Twitch.Stream] -> VarThread [(MLG.Stream, MLG.Channel)] -> VarThread [Azubu.Stream] -> VarThread [Hitbox.Stream] -> STM [Stream]
combine whitelist va vb vc vd = do
  wl <- readVarThread whitelist
  mconcat <$> sequence
    [ fmap (fmap TStream . allowed wl) $ readVarThread va
    , MStream <$$> readVarThread vb
    , AStream <$$> readVarThread vc
    , HStream <$$> readVarThread vd ]
  where
    f <$$> a = fmap f <$> a

retrieveShortened :: Googl.APIKey -> TVar URLMap -> Text -> IO Text
retrieveShortened googlKey urlMap longURL =
  Map.lookup longURL <$> atomically (readTVar urlMap) >>= \case
    Just x -> return x
    Nothing ->
      Googl.shortenURL (Just googlKey) longURL >>= \case
        Right (Googl.ShortURL s _) -> do
          atomically $ modifyTVar urlMap (Map.insert longURL s)
          return s
        Left _ -> return longURL

wikiPageTracker :: Options -> (Text -> STM ()) -> IO ()
wikiPageTracker (Options (Username u) p r _ _ _) send = forever $ do
  runReddit u p (getWikiPage r "sidebar") >>= \case
    Left _ -> return ()
    Right wp -> atomically $ send $ contentMarkdown wp
  threadDelay $ 5 * 60 * 1000 * 1000

whitelistTracker :: Options -> (Whitelist -> STM ()) -> IO ()
whitelistTracker (Options (Username u) p r _ _ _) send = do
  atomically $ send mempty
  forever $ do
    runReddit u p (getWikiPage r "streamer_whitelist") >>= \case
      Left _ -> return ()
      Right wp -> case decodeEither $ encodeUtf8 $ contentMarkdown wp of
        Left _ -> return ()
        Right x -> atomically $ send $ Set.fromList $ map Text.toLower x
    threadDelay $ 15 * 60 * 1000 * 1000

formatStreams :: [Stream] -> Text
formatStreams = Text.intercalate "\n\n>>[](#separator)\n\n" . zipWith formatStream [0..]

formatStream :: Int -> Stream -> Text
formatStream n (TStream stream) =
  mconcat [ ">>>#[", Text.strip $ Twitch.title stream, "]"
          , "(", Twitch.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Twitch.viewers stream, " @ ", Twitch.streamer stream ]
formatStream n (MStream (s, c)) =
  mconcat [ ">>>#[", Text.strip $ MLG.channelSubtitle c, "]"
          , "(", MLG.channelURL c, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ MLG.streamViewers s, " @ ", MLG.channelName c ]
formatStream n (AStream stream) =
  mconcat [ ">>>#[", Text.strip $ Azubu.title stream, "]"
          , "(", Azubu.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Azubu.viewers stream, " @ ", Azubu.streamer stream ]
formatStream n (HStream stream) =
  mconcat [ ">>>#[", Text.strip $ Hitbox.title stream, "]"
          , "(", Hitbox.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Hitbox.viewers stream, " @ ", Hitbox.streamer stream ]

formatMatches :: UTCTime -> [(Match, Text)] -> Text
formatMatches u = Text.intercalate "\n\n[](#separator)\n\n" . map (uncurry $ formatMatch u)

formatMatch :: UTCTime -> Match -> Text -> Text
formatMatch time match shortURL = execWriter $ do
  tell ">>>[~~"
  tell $ Gosu.tournamentName $ Gosu.tournament match
  tell "~~\n~~"
  if Gosu.dateTime match < time || Gosu.isLive match
    then tell "LIVE"
    else tell $ humanReadableTime $ floor $ Gosu.dateTime match `diffUTCTime` time
  tell "~~\n~~"
  tell $ Gosu.name $ Gosu.firstOpponent match
  tell "~~\n~~"
  tell $ Gosu.name $ Gosu.secondOpponent match
  tell "~~]("
  tell shortURL
  tell ")\n"
  tell "[](/"
  tell $ Text.toLower $ Gosu.countryCode $ Gosu.firstOpponent match
  tell ")\n"
  tell "[](/"
  tell $ Text.toLower $ Gosu.countryCode $ Gosu.secondOpponent match
  tell ")\n"
