module Bot.Args where

import Sources.Gosu

import Data.Text (Text)
import Data.Yaml hiding (Parser)
import Options.Applicative
import Reddit.Types.Subreddit
import Reddit.Types.User
import WebAPI.Dota
import qualified Googl

type Password = Text

data Options =
  Options Username Password SubredditName Bans
  deriving (Show)

instance FromJSON Options where
  parseJSON (Object o) =
    Options <$> o .: "username"
            <*> o .: "password"
            <*> o .: "subreddit"
            <*> (Bans <$> (o .:? "banned" .!= []))
  parseJSON _ = mempty

newtype Bans = Bans [Text]
  deriving (Show, Read, Eq)

data CmdOptions = CmdOptions FilePath

getOpts :: IO CmdOptions
getOpts = execParser $ info (helper <*> cmdopts) fullDesc

cmdopts :: Parser CmdOptions
cmdopts = fmap CmdOptions $ argument str $ mconcat
  [ metavar "CONFIG_FILENAME"
  , help configLine ]

configLine :: String
configLine = unlines
  [ "File that should be parsed for configuring the bot."
  , "  The file should be a valid Yaml file with a single object with"
  , "  up to six fields as follows:"
  , "  'username': the Reddit username that the bot should use"
  , "    for authentication. This account should be a moderator"
  , "    on the subreddit name given in the 'subreddit' field."
  , "  'password': the corresponding Reddit password for the given"
  , "    account."
  , "  'subreddit': the subreddit whose sidebar the bot should operate"
  , "    on. Make sure that the given user has config edit permissions."
  , "  'gg_key': the GosuGamers API key for the bot to use. Leave this"
  , "    blank if you don't have one, and the bot will run with match"
  , "    ticker functionality disabled."
  , "  'valve_key': the Steam WebAPI key to use for prize-pool tracking."
  , "    Leave this blank if you don't have one, and the bot won't"
  , "    update the prize pool."
  , "  'googl_key': the Google link shortener key for shrinking match"
  , "    page URLs. Leave this blank if you don't have one, and the bot"
  , "    will simply use the longer URL." ]
