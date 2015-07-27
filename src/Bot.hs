module Bot (main, run) where

import Bot.Args
import Bot.Utils
import Control.Concurrent.VarThread
import Sources.Gosu
import Sources.PrizePool
import Sources.PlayerCount
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
import Data.Monoid
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Text (Text)
import Data.Time.Clock
import Data.Yaml
import Prelude
import Reddit hiding (Options)
import Reddit.Types.Wiki
import System.Exit
import qualified Data.Map as Map
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
  pc <- playersTrackerThread
  wp <- newEmptyVarThread (wikiPageTracker o)
  ts <- Twitch.twitchTrackerThread
  as <- Azubu.azubuTrackerThread
  hs <- Hitbox.hitboxTrackerThread
  mlgs <- MLG.mlgTrackerThread
  urlmap <- newTVarIO Map.empty
  mp <- Gosu.gosuTrackerThread gg
  forever $ do
    (prize, alive, wikiText, streams, ms) <- atomically $
      (,,,,) <$> readVarThread pp
             <*> readVarThread pc
             <*> readVarThread wp
             <*> combine ts mlgs as hs
             <*> readVarThread mp
    void $ runReddit u p $ do
      currentTime <- liftIO getCurrentTime
      shorteneds <- mapM (liftIO . retrieveShortened gk urlmap . Gosu.matchURL) ms
      app <- return $ mconcat $ map Endo
        [ Text.replace "%%PRIZE%%" $ "$" <> thousandsFormat prize
        , Text.replace "%%STREAMS%%" $ formatStreams $ take 5 $ sortBy (comparing (Down . streamViewers)) streams
        , Text.replace "%%MATCHES%%" $ formatMatches currentTime $ ms `zip` shorteneds
        , Text.replace "%%SERVERS%%" $ if alive then "OK" else "Offline"
        , Text.replace "%%ANNOUNCE%%" $ if alive then "" else "1. [Dota 2 Servers are Offline](http://steamstat.us#notice)"]
      editWikiPage r "config/sidebar" (appEndo app wikiText) "sidebar update"
    threadDelay $ 60 * 1000 * 1000

combine :: VarThread [Twitch.Stream] -> VarThread [(MLG.Stream, MLG.Channel)] -> VarThread [Azubu.Stream] -> VarThread [Hitbox.Stream] -> STM [Stream]
combine va vb vc vd =
  mconcat <$> sequence
    [ TStream <$$> readVarThread va
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
