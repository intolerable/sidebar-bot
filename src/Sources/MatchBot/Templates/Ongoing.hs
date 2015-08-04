module Sources.MatchBot.Templates.Ongoing where

import Sources.MatchBot.Templates.Flairs

import Control.Lens
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Text.Format
import WebAPI.Dota.Types.Match
import WebAPI.Dota.Types.Player
import WebAPI.Dota.Types.Scoreboard
import qualified Data.Text.Lazy as Lazy

makeOutput :: LiveLeagueMatch -> Lazy.Text
makeOutput g = execWriter $ do
  tellBlock $ maybe "Pre-game" (format "Last updated: {}" . Only. lastUpdateTime) $ g ^? scoreboard.traverse.duration
  tellBlock $ teamsTable g
  tellBlock $ bansTable g
  tellBlock $ picksTable g
  tellBlock $ scoreTable g
  tellBlock $ otherLinks g

currentScore :: LiveLeagueMatch -> Lazy.Text
currentScore g = format "Current series score: {}-{}" (g ^. radiantSeriesWins, g ^. direSeriesWins)

otherLinks :: LiveLeagueMatch -> Lazy.Text
otherLinks g = Lazy.intercalate "\n\n"
  [ format "More information on [TrackDota](http://trackdota.com/matches/{})" $ Only m
  , "## UPDATING LIVE" ]
  where MatchID m = g ^. identifier

teamsTable :: LiveLeagueMatch -> Lazy.Text
teamsTable g = format fmtString $
  ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.traverse.name.traverse
  , fromMaybe 0 $ g ^? scoreboard.traverse.radiantTeam.traverse.score
  , fromMaybe 0 $ g ^? scoreboard.traverse.direTeam.traverse.score
  , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.traverse.name.traverse )
  where
    fmtString = mconcat
      [ "| Team | Score | vs. | Score | Team |\n"
      , "|:----:|:-----:|:---:|:-----:|:----:|\n"
      , "| {} | {} | vs. | {} | {} |" ]

scoreTable :: LiveLeagueMatch -> Lazy.Text
scoreTable g = Lazy.intercalate "\n"
  [ format "| Player | Level | Hero | K/D/A | vs. | K/D/A | Hero | Level | Player |" ()
  , format "|-------:|:-----:|:----:|:-----:|:---:|:-----:|:----:|:-----:|:-------|" ()
  , playerLine g 0
  , playerLine g 1
  , playerLine g 2
  , playerLine g 3
  , playerLine g 4 ]

playerLine :: LiveLeagueMatch -> Int -> Lazy.Text
playerLine g n =
  format "| {} | {} | {} | {} | vs. | {} | {} | {} | {} |"
    ( Lazy.replace "|" "&#124;" $ fromMaybe "" $ findPlayerName g =<< (g ^? scoreboard.traverse.radiantTeam.traverse.players.ix n.accountID)
    , maybe "" (format "{}" . Only) $ g ^? scoreboard.traverse.radiantTeam.traverse.players.ix n.level
    , fmap heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.players.ix n.heroID
    , maybe "-" (format "{}/{}/{}") $ g ^? scoreboard.traverse.radiantTeam.traverse.players.ix n.kda
    , maybe "-" (format "{}/{}/{}") $ g ^? scoreboard.traverse.direTeam.traverse.players.ix n.kda
    , fmap heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.players.ix n.heroID
    , maybe "" (format "{}" . Only) $ g ^? scoreboard.traverse.direTeam.traverse.players.ix n.level
    , Lazy.replace "|" "&#124;" $ fromMaybe "" $ findPlayerName g =<< (g ^? scoreboard.traverse.direTeam.traverse.players.ix n.accountID) )

findPlayerName :: LiveLeagueMatch -> AccountID -> Maybe Lazy.Text
findPlayerName g a = fmap Lazy.fromStrict $ g ^? players.traverse.filtered (\x -> view identifier x == a).name

bansTable :: LiveLeagueMatch -> Lazy.Text
bansTable g = Lazy.intercalate "\n"
  [ format "| Team | Bans  | vs. |  Bans | Team |" ()
  , format "|:----:|:-----:|:---:|:-----:|:----:|" ()
  , format "| {}   | {} {} | vs. | {} {} | {}   |"
    ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.traverse.name.traverse
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.bans.traverse.ix 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.bans.traverse.ix 1
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.bans.traverse.ix 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.bans.traverse.ix 1
    , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.traverse.name.traverse )
  , format "|      | {} {} |     | {} {} |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.bans.traverse.ix 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.bans.traverse.ix 3
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.bans.traverse.ix 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.bans.traverse.ix 3 )
  , format "|      |    {} |     | {}    |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.bans.traverse.ix 4
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.bans.traverse.ix 4 ) ]

picksTable :: LiveLeagueMatch -> Lazy.Text
picksTable g = Lazy.intercalate "\n"
  [ format "| Team | Picks | vs. | Picks | Team |" ()
  , format "|:----:|:-----:|:---:|:-----:|:----:|" ()
  , format "| {}   | {} {} | vs. | {} {} | {}   |"
    ( maybe "[](/logo-blank)" teamFlag $ g ^? radiantTeam.traverse.name.traverse
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.picks.traverse.ix 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.picks.traverse.ix 1
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.picks.traverse.ix 0
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.picks.traverse.ix 1
    , maybe "[](/logo-blank)" teamFlag $ g ^? direTeam.traverse.name.traverse )
  , format "|      | {} {} |     | {} {} |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.picks.traverse.ix 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.picks.traverse.ix 3
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.picks.traverse.ix 2
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.picks.traverse.ix 3 )
  , format "|      |    {} |     | {}    |      |"
    ( maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.radiantTeam.traverse.picks.traverse.ix 4
    , maybe "[](/logo-blank)" heroFlair $ g ^? scoreboard.traverse.direTeam.traverse.picks.traverse.ix 4 ) ]
