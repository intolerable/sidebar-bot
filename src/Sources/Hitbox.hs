module Sources.Hitbox where

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
import Text.Read

data Stream = Stream { streamer :: Text
                     , title :: Text
                     , viewers :: Integer
                     , url :: Text
                     , categoryID :: Maybe Text }
  deriving (Show, Read, Eq)

instance FromJSON Stream where
  parseJSON (Object o) = do
    c <- o .: "channel"
    Stream <$> o .: "media_display_name"
           <*> o .: "media_status"
           <*> (o .: "media_views" >>= readM)
           <*> c .: "channel_link"
           <*> o .:? "category_seo_key"
  parseJSON _ = mempty

readM :: (Monad m, Read a) => String -> m a
readM x = case readMaybe x of
  Just r -> return r
  Nothing -> fail "read failed"

data StreamList = StreamList [Stream]
  deriving (Show, Eq)

instance FromJSON StreamList where
  parseJSON (Object o) =
    StreamList <$> o .: "livestream"
  parseJSON _ = mempty

instance Receivable StreamList where
  receive = useFromJSON

azubuAPI :: Builder
azubuAPI = basicBuilder "Azubu" "https://api.hitbox.tv"

streamsRoute :: Route
streamsRoute = Route [ "media", "live", "list" ]
                     [ ]
                     "GET"

getStreams :: IO (Either (APIError ()) StreamList)
getStreams = execAPI azubuAPI () $ runRoute streamsRoute

hitboxTrackerThread :: IO (VarThread [Stream])
hitboxTrackerThread = newEmptyVarThread $ \update ->
  forever $ do
    getStreams >>= \case
      Left _ -> return ()
      Right (StreamList ss) -> atomically $ update $ filter isDota ss
    threadDelay $ 30 * 1000 * 1000

isDota :: Stream -> Bool
isDota (Stream _ _ _ _ (Just "dota-2")) = True
isDota _ = False
