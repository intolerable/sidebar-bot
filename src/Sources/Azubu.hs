module Sources.Azubu where

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
    u <- o .: "user"
    Stream <$> u .: "display_name"
           <*> u .: "alt_name"
           <*> o .: "view_count"
           <*> o .: "url_channel"
  parseJSON _ = mempty

data StreamList = StreamList [Stream]
  deriving (Show, Eq)

instance FromJSON StreamList where
  parseJSON (Object o) =
    StreamList <$> o .: "data"
  parseJSON _ = mempty

instance Receivable StreamList where
  receive = useFromJSON

azubuAPI :: Builder
azubuAPI = basicBuilder "Azubu" "https://api.azubu.tv"

streamsRoute :: Route
streamsRoute = Route [ "public", "channel", "live", "list", "game", "dota-2" ]
                     [ ]
                     "GET"

getStreams :: IO (Either (APIError ()) StreamList)
getStreams = execAPI azubuAPI () $ runRoute streamsRoute

azubuTrackerThread :: IO (VarThread [Stream])
azubuTrackerThread = newEmptyVarThread $ \update ->
  forever $ do
    getStreams >>= \case
      Left _ -> return ()
      Right (StreamList ss) -> atomically $ update ss
    threadDelay $ 30 * 1000 * 1000
