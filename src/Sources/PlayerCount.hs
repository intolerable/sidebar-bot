module Sources.PlayerCount where

import Control.Concurrent.VarThread

import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Data.Aeson
import Data.Monoid
import Network.API.Builder

newtype PlayerCount = PlayerCount Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON PlayerCount where
  parseJSON (Object o) = PlayerCount <$> o .: "rgPlayersTotal"
  parseJSON _ = mempty

instance Receivable PlayerCount where receive = useFromJSON

playerCount :: Builder
playerCount = basicBuilder "Valve player count" "https://www.dota2.com/overview/externaldata"

playerCountRoute :: Route
playerCountRoute = Route [] [] "GET"

getPlayerCount :: IO (Either (APIError ()) PlayerCount)
getPlayerCount = execAPI playerCount () (runRoute playerCountRoute)

playersTrackerThread :: IO (VarThread Bool)
playersTrackerThread = newEmptyVarThread $ \update ->
  forever $ do
    getPlayerCount >>= \case
      Left err -> putStrLn $ "Playercount error:" <> show err
      Right (PlayerCount x) ->
        atomically $ update $ x /= 0
    threadDelay $ 60 * 1000 * 1000
