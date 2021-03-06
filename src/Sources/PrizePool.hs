module Sources.PrizePool where

import Control.Concurrent.VarThread

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.STM
import Data.Monoid
import WebAPI.Dota
import WebAPI.Dota.Types.League

prizeTrackerThread :: WebAPIKey -> IO (VarThread Integer)
prizeTrackerThread key = newEmptyVarThread $ \update ->
  forever $ do
    runWebAPI key (Language $ pure "en") (getPrizePool (LeagueID 4664)) >>= \case
      Left err -> putStrLn $ "Prizepool error:" <> show err
      Right pp -> atomically $ update $ view amount pp
    threadDelay $ 5 * 60 * 1000 * 1000
