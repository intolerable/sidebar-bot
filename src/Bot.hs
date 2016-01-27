module Bot (main, run) where

import Bot.Args
import Bot.Utils
import Control.Concurrent.VarThread
import qualified Sources.Azubu as Azubu
import qualified Sources.Hitbox as Hitbox
import qualified Sources.MLG as MLG
import qualified Sources.Twitch as Twitch

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Monoid
import Data.List
import Data.Ord
import Data.Text (Text)
import Data.Yaml
import Prelude
import Reddit hiding (Options)
import Reddit.Types.Wiki
import System.Exit
import qualified Data.Text as Text

data Stream = TStream Twitch.Stream
            | MStream (MLG.Stream, MLG.Channel)
            | AStream Azubu.Stream
            | HStream Hitbox.Stream
  deriving (Show, Read, Eq)

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
redesign o@(Options (Username u) p r) = do
  wp <- newEmptyVarThread (wikiPageTracker o)
  ts <- Twitch.twitchTrackerThread
  forever $ do
    (wikiText, streams) <- atomically $
      (,) <$> readVarThread wp
          <*> readVarThread ts
    void $ runReddit u p $ do
      let modifyText = Text.replace "%%STREAMS%%" $ formatStreams $ map TStream $ take 5 $ sortBy (comparing (Down . Twitch.viewers)) streams
      editWikiPage r "config/sidebar" (modifyText wikiText) "sidebar update"
    threadDelay $ 60 * 1000 * 1000

wikiPageTracker :: Options -> (Text -> STM ()) -> IO ()
wikiPageTracker (Options (Username u) p r) send = forever $ do
  runReddit u p (getWikiPage r "sidebar") >>= \case
    Left _ -> return ()
    Right wp -> atomically $ send $ contentMarkdown wp
  threadDelay $ 5 * 60 * 1000 * 1000

formatStreams :: [Stream] -> Text
formatStreams = Text.intercalate "\n\n" . zipWith formatStream [0..]

formatStream :: Int -> Stream -> Text
formatStream n (TStream stream) =
  mconcat [ ">>>#[", fixUntitled $ Text.strip $ Twitch.title stream, "]"
          , "(", Twitch.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Twitch.viewers stream, " viewers @ ", Twitch.streamer stream ]
formatStream n (MStream (s, c)) =
  mconcat [ ">>>#[", fixUntitled $ Text.strip $ MLG.channelSubtitle c, "]"
          , "(", MLG.channelURL c, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ MLG.streamViewers s, " viewers @ ", MLG.channelName c ]
formatStream n (AStream stream) =
  mconcat [ ">>>#[", fixUntitled $ Text.strip $ Azubu.title stream, "]"
          , "(", Azubu.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Azubu.viewers stream, " viewers @ ", Azubu.streamer stream ]
formatStream n (HStream stream) =
  mconcat [ ">>>#[", fixUntitled $ Text.strip $ Hitbox.title stream, "]"
          , "(", Hitbox.url stream, "#profile-", tshow n, ")\n"
          , ">##\n"
          , ">###", tshow $ Hitbox.viewers stream, " viewers @ ", Hitbox.streamer stream ]

fixUntitled :: Text -> Text
fixUntitled "" = "Untitled Broadcast"
fixUntitled x = x
