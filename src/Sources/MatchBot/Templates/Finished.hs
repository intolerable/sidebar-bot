module Sources.MatchBot.Templates.Finished where

import Sources.MatchBot.Templates.Flairs

import Control.Lens
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Text.Format
import WebAPI.Dota.Types.Match
import WebAPI.Dota.Types.Player
import WebAPI.Dota.Types.Team
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy

makePostgame :: Match -> Lazy.Text
makePostgame g = execWriter $ do
  tellBlock ""
  case g ^. winner of
    Radiant -> tellBlock $ maybe "Radiant" (format "## {} Victory!" . Only) $ g ^? radiantTeam.name
    Dire -> tellBlock $ maybe "Dire" (format "## {} Victory!" . Only) $ g ^? direTeam.name
  tellBlock $ format "## Duration: {}" $ Only $ lastUpdateTime (g ^. duration)
  tellBlock $ teamsTablePost g
  tellBlock $ scoreTablePost g
  tellBlock $ otherLinksPost g

otherLinksPost :: Match -> Lazy.Text
otherLinksPost g = Lazy.intercalate "\n\n"
  [ format "More information on [Dotabuff](http://dotabuff.com/matches/{}), [YASP](http://yasp.co/matches/{}), and [datDota](http://datdota.com/match.php?q={})" (m, m, m) ]
  where MatchID m = g ^. identifier

teamsTablePost :: Match -> Lazy.Text
teamsTablePost g = format fmtString $
  ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.name.traverse
  , sumOf (direPlayers.deaths) g
  , sumOf (radiantPlayers.deaths) g
  , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.name.traverse )
  where
    fmtString = mconcat
      [ "| Team | Score | vs. | Score | Team |\n"
      , "|:----:|:-----:|:---:|:-----:|:----:|\n"
      , "| {} | {} | vs. | {} | {} |" ]


scoreTablePost :: Match -> Lazy.Text
scoreTablePost g = Lazy.intercalate "\n"
  [ format "| Hero | Player | Level | K/D/A | Gold spent | CS | GPM | XPM |" ()
  , format "|:------:|:-----:|:----:|:-----:|:---:|:-----:|:----:|:-----:|" ()
  , playerLinePost radiantPlayers g 0
  , playerLinePost radiantPlayers g 1
  , playerLinePost radiantPlayers g 2
  , playerLinePost radiantPlayers g 3
  , playerLinePost radiantPlayers g 4
  , "| | | | | | | | |"
  , teamLinePost radiantPlayers g
  , playerLinePost direPlayers g 0
  , playerLinePost direPlayers g 1
  , playerLinePost direPlayers g 2
  , playerLinePost direPlayers g 3
  , playerLinePost direPlayers g 4
  , teamLinePost direPlayers g ]

teamLinePost :: Traversal' Match Player -> Match -> Lazy.Text
teamLinePost ps g =
  format "| | | **{}** | **{}** | **{}** | **{}** | **{}** | **{}** |"
    ( sumOf (traverse.level) $ g ^.. ps
    , format "{}/{}/{}" (sumOf (traverse.kills) $ g ^.. ps, sumOf (traverse.deaths) $ g ^.. ps, sumOf (traverse.assists) $ g ^.. ps)
    , sumOf (traverse.goldSpent) $ g ^.. ps
    , format "{}/{}" (sumOf (traverse.lastHits) $ g ^.. ps, sumOf (traverse.denies) $ g ^.. ps)
    , sumOf (traverse.gpm) $ g ^.. ps
    , sumOf (traverse.xpm) $ g ^.. ps )

playerLinePost :: Traversal' Match Player -> Match -> Int -> Lazy.Text
playerLinePost ps g n =
  format "| {} | {} | {} | {} | {} | {} | {} | {} |"
    ( fmap heroFlair $ (g ^.. ps) ^? ix n.heroID
    , Lazy.replace "|" "&#124;" $ fromMaybe "" $ (g ^.. ps) ^? ix n.accountID >>= \i -> Map.lookup i playerOfficialNames
    , (g ^.. ps) ^? ix n.level
    , maybe "-" (format "{}/{}/{}") $ (g ^.. ps) ^? ix n.kda
    , (g ^.. ps) ^? ix n.goldSpent
    , maybe "-" (format "{}/{}") $ (g ^.. ps) ^? ix n.cs
    , (g ^.. ps) ^? ix n.gpm
    , (g ^.. ps) ^? ix n.xpm )
