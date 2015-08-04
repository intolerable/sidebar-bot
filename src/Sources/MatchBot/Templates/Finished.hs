module Sources.MatchBot.Templates.Finished where

import Sources.MatchBot.Templates.Flairs

import Control.Lens
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Text.Format
import WebAPI.Dota.Types.Hero
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
  tellBlock $ bansTablePost g
  tellBlock $ picksTablePost g
  tellBlock $ scoreTablePost g
  tellBlock $ otherLinksPost g

otherLinksPost :: Match -> Lazy.Text
otherLinksPost g = Lazy.intercalate "\n\n"
  [ format "More information on [Dotabuff](http://dotabuff.com/matches/{}), [YASP](http://yasp.co/matches/{}), and [datDota](http://datdota.com/match.php?q={})" (m, m, m) ]
  where MatchID m = g ^. identifier

bansTablePost :: Match -> Lazy.Text
bansTablePost g = Lazy.intercalate "\n"
  [ format "| Team | Bans  | vs. |  Bans | Team |" ()
  , format "|:----:|:-----:|:---:|:-----:|:----:|" ()
  , format "| {}   | {} {} | vs. | {} {} | {}   |"
    ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.name.traverse
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant False 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant False 1
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire False 1
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire False 0
    , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.name.traverse )
  , format "|      | {} {} |     | {} {} |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant False 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant False 3
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire False 3
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire False 2 )
  , format "|      |    {} |     | {}    |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant False 4
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire False 4 ) ]

picksTablePost :: Match -> Lazy.Text
picksTablePost g = Lazy.intercalate "\n"
  [ format "| Team | Picks | vs. | Picks | Team |" ()
  , format "|:----:|:-----:|:---:|:-----:|:----:|" ()
  , format "| {}   | {} {} | vs. | {} {} | {}   |"
    ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.name.traverse
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant True 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant True 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire True 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire True 1
    , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.name.traverse )
  , format "|      | {} {} |     | {} {} |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant True 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant True 3
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire True 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire True 3 )
  , format "|      |    {} |     | {}    |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Radiant True 4
    , maybe "[](/logo-blank)" heroFlair $ g ^? draftNth Dire True 4 ) ]

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
    , g ^? elementOf ps n.level
    , maybe "-" (format "{}/{}/{}") $ g ^? elementOf ps n.kda
    , g ^? elementOf ps n.goldSpent
    , maybe "-" (format "{}/{}") $ g ^? elementOf ps n.cs
    , g ^? elementOf ps n.gpm
    , g ^? elementOf ps n.xpm )

draftNth :: Faction -> Bool -> Int -> Traversal' Match HeroID
draftNth t p n = elementOf (draft.traverse.filtered (\x -> view faction x == t && view isPick x == p)) n.heroID
