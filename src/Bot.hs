module Bot (main, run) where

import Bot.Utils
import Control.Concurrent.VarThread
import Sources.PrizePool
import Sources.Gosu
import qualified Sources.MLG as MLG
import qualified Sources.Gosu as Gosu
import qualified Sources.Twitch as Twitch

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Text (Text)
import Data.Time.Clock
import Data.Yaml
import Options.Applicative
import Prelude
import Reddit
import Reddit.Types.Subreddit (SubredditName(..))
import Reddit.Types.Wiki
import Reddit.Types.User (Username(..))
import System.Environment
import System.Exit
import WebAPI.Dota
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Googl

data Stream = TStream Twitch.Stream
            | MStream (MLG.Stream, MLG.Channel)
  deriving (Show, Read, Eq)

streamViewers :: Stream -> Integer
streamViewers (TStream s) = Twitch.viewers s
streamViewers (MStream (s, _)) = MLG.streamViewers s

type URLMap = Map Text Text

type Password = Text

data Options =
  Options Username Password SubredditName GosuGamersKey WebAPIKey Googl.APIKey
  deriving (Show)

instance FromJSON Options where
  parseJSON (Object o) =
    Options <$> o .: "username"
            <*> o .: "password"
            <*> o .: "subreddit"
            <*> (GGKey <$> o .: "gg_key")
            <*> (WebAPIKey <$> o .: "valve_key")
            <*> (Googl.APIKey <$> o .: "googl_key")
  parseJSON _ = mempty

main :: IO ()
main = getArgs >>= \case
  [fp] -> run fp
  _ -> putStrLn "invalid args"

run :: FilePath -> IO ()
run fp =
  decodeFileEither fp >>= \case
    Left err -> do
      print err
      exitFailure
    Right x -> redesign x

redesign :: Options -> IO ()
redesign o@(Options (Username u) p r gg wk gk) = do
  pp <- prizeTrackerThread wk
  wp <- newEmptyVarThread (wikiPageTracker o)
  ts <- Twitch.twitchTrackerThread
  mlgs <- MLG.mlgTrackerThread
  urlmap <- newTVarIO Map.empty
  mp <- Gosu.gosuTrackerThread gg
  forever $ do
    (prize, wikiText, streams, ms) <- atomically $
      (,,,) <$> readVarThread pp
            <*> readVarThread wp
            <*> combine ts mlgs
            <*> readVarThread mp
    void $ runRedditWithRateLimiting u p $ do
      currentTime <- liftIO getCurrentTime
      shorteneds <- mapM (liftIO . retrieveShortened gk urlmap . Gosu.matchURL) ms
      let newText = Text.replace "%%PRIZE%%" ("$" <> thousandsFormat prize) wikiText
      let newText' = Text.replace "%%STREAMS%%" (formatStreams $ take 5 $ sortBy (comparing (Down . streamViewers)) streams) newText
      let newText'' = Text.replace "%%MATCHES%%" (formatMatches currentTime (ms `zip` shorteneds)) newText'
      editWikiPage r "config/sidebar" newText'' "sidebar update"
    threadDelay $ 60 * 1000 * 1000

combine :: VarThread [Twitch.Stream] -> VarThread [(MLG.Stream, MLG.Channel)] -> STM [Stream]
combine va vb =
  mappend <$> (fmap TStream <$> readVarThread va) <*> (fmap MStream <$> readVarThread vb)

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
  runRedditWithRateLimiting u p (getWikiPage r "sidebar") >>= \case
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
