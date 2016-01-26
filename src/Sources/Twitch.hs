module Sources.Twitch
  ( getStreams
  , Stream(..)
  , StreamList(..)
  , twitchTrackerThread ) where

import Control.Concurrent.VarThread

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import Prelude

data Stream = Stream { streamer :: Text
                     , title :: Text
                     , viewers :: Integer
                     , url :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Stream where
  parseJSON (Object o) = do
    channel <- o .: "channel"
    Stream <$> channel .: "display_name"
           <*> (channel .: "status" <|> pure "(No title)")
           <*> o .: "viewers"
           <*> channel .: "url"
  parseJSON _ = mempty

data StreamList = StreamList [Stream]
  deriving (Show, Read, Eq)

instance FromJSON StreamList where
  parseJSON (Object o) =
    StreamList <$> o .: "streams"
  parseJSON _ = mempty

instance Receivable StreamList where
  receive = useFromJSON

twitchAPI :: Builder
twitchAPI = basicBuilder "Twitch" "https://api.twitch.tv/kraken"

streamsRoute :: Route
streamsRoute = Route [ "streams" ]
                     [ "game" =. ("Summoners War: Sky Arena" :: Text)
                     , "limit" =. (5 :: Integer) ]
                     "GET"

getStreams :: IO (Either (APIError ()) StreamList)
getStreams = execAPI twitchAPI () $ runRoute streamsRoute

twitchTrackerThread :: IO (VarThread [Stream])
twitchTrackerThread = newEmptyVarThread $ \update ->
  forever $ do
    getStreams >>= \case
      Left err -> putStrLn $ "Twitch error:" <> show err
      Right (StreamList ss) -> atomically $ update ss
    threadDelay $ 60 * 1000 * 1000
