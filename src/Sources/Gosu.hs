module Sources.Gosu
  ( getMatches
  , GosuGamersKey(..)
  , Matches(..)
  , Match(..)
  , Team(..)
  , Tournament(..)
  , gosuTrackerThread ) where

import Control.Concurrent.VarThread

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.API.Builder
import Prelude
import qualified Data.Map as Map
import qualified Data.Time.Format as Time

gosuTrackerThread :: GosuGamersKey -> IO (VarThread [Match])
gosuTrackerThread ggkey = newEmptyVarThread $ \update ->
  forever $ do
    getMatches ggkey >>= \case
      Left _ -> return ()
      Right (Matches ms) -> atomically $ update ms
    threadDelay $ 15 * 60 * 1000 * 1000

getMatches :: GosuGamersKey -> IO (Either (APIError ()) Matches)
getMatches ggkey = execAPI ggAPI () $ runRoute $ matchTickerRoute ggkey

ggAPI :: Builder
ggAPI = basicBuilder "GosuGamers" "http://www.gosugamers.net"

newtype GosuGamersKey = GGKey Text
  deriving (Show, Read, Eq)

instance ToQuery GosuGamersKey where
  toQuery k (GGKey v) = toQuery k v

matchTickerRoute :: GosuGamersKey -> Route
matchTickerRoute ggkey =
  Route [ "api", "matches" ]
        [ "apiKey" =. ggkey
        , "game" =. ("dota2" :: Text)
        , "maxResults" =. (6 :: Integer) ]
        "GET"

newtype Matches = Matches [Match]
  deriving (Show, Read, Eq)

instance FromJSON Matches where
  parseJSON (Object o) =
    Matches <$> o .: "matches"
  parseJSON _ = mempty

instance Receivable Matches where receive = useFromJSON

data Match = Match { matchID :: Integer
                   , isLive :: Bool
                   , dateTime :: UTCTime
                   , matchURL :: Text
                   , firstOpponent :: Team
                   , secondOpponent :: Team
                   , tournament :: Tournament }
  deriving (Show, Read, Eq)

instance FromJSON Match where
  parseJSON (Object o) =
    Match <$> o .: "id"
          <*> o .: "isLive"
          <*> (Time.parseTimeM True Time.defaultTimeLocale "%FT%T%z" =<< o .: "datetime")
          <*> o .: "pageUrl"
          <*> o .: "firstOpponent"
          <*> o .: "secondOpponent"
          <*> o .: "tournament"
  parseJSON _ = mempty

data Team = Team { name :: Text
                 , shortName :: Text
                 , countryCode :: Text
                 , teamUrl :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Team where
  parseJSON (Object o) =
    Team <$> o .: "name"
         <*> o .: "shortName"
         <*> fmap substituteCode ((o .: "country") >>= (.: "countryCode"))
         <*> o .: "pageUrl"
  parseJSON _ = mempty

substituteCode :: Text -> Text
substituteCode x = fromMaybe x $ Map.lookup x codeSwitches
  where
    codeSwitches = Map.fromList
      [ ("XA", "US") ]

data Tournament = Tournament { tournamentName :: Text
                             , tournamentUrl :: Text
                             , tournamentType :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Tournament where
  parseJSON (Object o) =
    Tournament <$> o .: "name"
               <*> o .: "pageUrl"
               <*> o .: "type"
  parseJSON _ = mempty
