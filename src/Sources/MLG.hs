module Sources.MLG
  ( getStreams
  , Channel(..)
  , Stream(..)
  , StreamList(..)
  , mlgTrackerThread ) where

import Control.Concurrent.VarThread

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import Prelude
import qualified Data.Map as Map
import qualified Data.Text as Text

data Stream =
  Stream { streamChannelID :: Integer
         , streamName :: Text
         , streamViewers :: Integer }
  deriving (Show, Read, Eq)

instance FromJSON Stream where
  parseJSON (Object o) =
    Stream <$> o .: "channel_id"
           <*> o .: "stream_name"
           <*> (o .: "viewers" <|> pure 0)
  parseJSON _ = mempty

newtype StreamList = StreamList [Stream]
  deriving (Show, Eq)

instance FromJSON StreamList where
  parseJSON (Object o) =
    StreamList <$> (o .: "data" >>= (.: "items"))
  parseJSON _ = mempty

instance Receivable StreamList where receive = useFromJSON

data Channel =
  Channel { channelID :: Integer
          , channelName :: Text
          , channelGameID :: Integer
          , channelSubtitle :: Text
          , channelURL :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Channel where
  parseJSON (Object o) =
    Channel <$> o .: "id"
            <*> o .: "name"
            <*> o .: "game_id"
            <*> o .: "subtitle"
            <*> o .: "url"
  parseJSON _ = mempty

newtype ChannelList = ChannelList [Channel]
  deriving (Show, Eq)

instance FromJSON ChannelList where
  parseJSON (Object o) =
    ChannelList <$> (o .: "data" >>= (.: "items"))
  parseJSON _ = mempty

instance Receivable ChannelList where receive = useFromJSON

mlgStreams :: Builder
mlgStreams = basicBuilder "MLG Streams" "https://streamapi.majorleaguegaming.com"

mlgChannels :: Builder
mlgChannels = basicBuilder "MLG Channels" "https://www.majorleaguegaming.com"

streamsRoute :: Route
streamsRoute = Route [ "service", "streams", "all" ]
                     [ "status" =. (1 :: Integer) ]
                     "GET"

channelInfo :: [Integer] -> Route
channelInfo ids = Route [ "api", "channels", "all" ]
                        [ "ids" =. ids
                        , "fields" =. Text.words "id name game_id url subtitle" ]
                        "GET"

getStreams :: IO (Either (APIError ()) [(Stream, Channel)])
getStreams = runEitherT $ do
  StreamList ss <- EitherT $
    execAPI mlgStreams () $ runRoute streamsRoute
  ChannelList cs <- EitherT $
    execAPI mlgChannels () $ runRoute $ channelInfo $ map streamChannelID ss
  return $ filter ((==20) . channelGameID . snd) $
    joinOn streamChannelID channelID ss cs

joinOn :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> [(a, b)]
joinOn f g as bs = Map.elems $ Map.intersectionWith (,) am bm
  where am = Map.fromList $ map (f &&& id) as
        bm = Map.fromList $ map (g &&& id) bs

mlgTrackerThread :: IO (VarThread [(Stream, Channel)])
mlgTrackerThread = newEmptyVarThread $ \update ->
  forever $ do
    getStreams >>= \case
      Left err -> putStrLn $ "MLG error:" <> show err
      Right mlgs -> atomically $ update mlgs
    threadDelay $ 60 * 1000 * 1000
