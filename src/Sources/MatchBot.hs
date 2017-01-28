module Sources.MatchBot where

import Sources.MatchBot.Templates.Finished
import Sources.MatchBot.Templates.Flairs
import Sources.MatchBot.Templates.Ongoing

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Format hiding (print)
import Data.Yaml
import Reddit hiding (bans, before, after)
import Reddit.Types.Message
import System.Environment
import System.Exit
import WebAPI.Dota
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Match
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Reddit.Types.Post as Post

lang :: Language
lang = Language $ Just "en"

subreddit :: SubredditName
subreddit = R "dota2"

main :: IO ()
main = do
  getArgs >>= \case
    (fmap Text.pack -> [user, pass, apikey]) ->
      go user pass (WebAPIKey apikey)
    _ -> do
      putStrLn "invalid arguments"
      exitFailure

go :: Text -> Text -> WebAPIKey -> IO ()
go user pass key = do
  withAsync (runR $ wikiCurrentMatches key) $ \a -> do
    void $ runR (messageHandler key runR)
    cancel a
  where
    runR x = do
      runRedditIndefinitely user pass x >>= \case
        Left err -> print err >> runR x
        Right res -> return (Right res)

wikiCurrentMatches :: MonadIO m => WebAPIKey -> RedditT m ()
wikiCurrentMatches key = forever $ do
  LiveMatchListing ms <-
    definitely $ liftIO $ runWebAPI key lang getLiveLeagueGames
  editWikiPage subreddit "live_matches" (Text.intercalate "\n\n" $ fmtGame <$> ms) "update current live games"
  liftIO $ threadDelay $ 15 * 1000 * 1000
  where
    fmtGame m =
      Lazy.toStrict $ format
        ( mconcat [ "{} vs. {}: [{}](http://trackdota.com/matches/{}) | "
                  , "[create new thread](https://www.reddit.com/message/compose/?to=d2tournamentthreads&subject=matchbot&message=match_id:%20{}%0A%0Atitle:%20TITLE_HERE) | "
                  , "[add to existing thread](https://www.reddit.com/message/compose/?to=d2tournamentthreads&subject=matchbot&message=match_id:%20{}%0A%0Apost_id:%20POST_ID)"] )
        ( maybe "[](/logo-blank)" teamFlag $ m ^? radiantTeam.traverse.name.traverse
        , maybe "[](/logo-blank)" teamFlag $ m ^? direTeam.traverse.name.traverse
        , (\(MatchID i) -> i) $ m ^. identifier
        , (\(MatchID i) -> i) $ m ^. identifier
        , (\(MatchID i) -> i) $ m ^. identifier
        , (\(MatchID i) -> i) $ m ^. identifier )

postThread :: MonadIO m => WebAPIKey ->  Text -> MatchID -> RedditT m ()
postThread key title mid = do
  pid <- submitSelfPost subreddit title ""
  void $ nest $ setPostFlair subreddit pid "Match | eSports" "match esports"
  setInboxReplies False pid
  keepUpdated key mid pid

keepUpdated :: MonadIO m => WebAPIKey -> MatchID -> PostID -> RedditT m ()
keepUpdated key mid pid = fix $ \loop -> do
  findLiveMatchInfo key mid >>= \case
    Nothing -> do
      res <- liftIO $ definitely $
        runWebAPI key (Language $ Just "en") $ getMatchDetails mid
      post <- getPostInfo pid
      case Post.content post of
        Post.TitleOnly -> do
          editPost pid $ Lazy.toStrict $
            wrapBlock "match-details" $ makePostgame res
        Post.Link _ -> return ()
        Post.SelfPost md _ -> do
          editPost pid $ replaceBlock "match-details" md $
            Lazy.toStrict $ makePostgame res
    Just g -> do
      post <- getPostInfo pid
      case Post.content post of
        Post.TitleOnly ->
          editPost pid $ Lazy.toStrict $ wrapBlock "match-details" $ makeOutput g
        Post.Link _ -> return ()
        Post.SelfPost md _ ->
          editPost pid $ replaceBlock "match-details" md $ Lazy.toStrict $ makeOutput g
      liftIO $ threadDelay $ 15 * 1000 * 1000
      loop

findLiveMatchInfo :: MonadIO m => WebAPIKey -> MatchID -> m (Maybe LiveLeagueMatch)
findLiveMatchInfo key mid = liftIO $ do
  LiveMatchListing ms <-
    definitely $ liftIO $ runWebAPI key lang getLiveLeagueGames
  return $ find (\m -> view identifier m == mid) ms

definitely :: MonadIO m => m (Either a b) -> m b
definitely x = fix $ \loop ->
  x >>= \case
    Left _ -> do
      liftIO $ threadDelay $ 5 * 1000 * 1000
      loop
    Right res -> return res

runRedditIndefinitely :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runRedditIndefinitely u p reddit = do
  let redditOpts = defaultRedditOptions { loginMethod = Credentials u p
                                        , customUserAgent = Just "/r/Dota2 live match threads bot" }
  runResumeRedditWith redditOpts reddit >>= \case
    Right x -> return $ Right x
    Left (HTTPError err, Just resume) -> do
      liftIO $ do
        print err
        putStrLn "http error, continuing"
        threadDelay $ 300 * 1000 * 1000
      runRedditIndefinitely u p resume
    Left (APIError (RateLimitError n _), Just resume) -> do
      liftIO $ putStrLn "rate limited, waiting"
      liftIO $ threadDelay $ fromIntegral n * 1000 * 1000
      liftIO $ putStrLn "rate limited, continuing"
      runRedditIndefinitely u p resume
    Left (x, _) -> do
      liftIO $ putStrLn "no resume possible!"
      return $ Left x

messageHandler :: MonadIO m => WebAPIKey -> (RedditT m () -> IO (Either (APIError RedditError) a)) -> RedditT m ()
messageHandler key runR = ($ Set.empty) $ fix $ \loop x -> do
  Listing _ _ ms <- getUnread
  let news = filter (\m -> not $ Set.member (messageID m) x) ms
  forM_ news $ \newMessage -> do
    when (isPrivateMessage newMessage) $ do
      case subject newMessage of
        "matchbot" -> do
          when (Set.member (from newMessage) moderators) $ do
            markRead $ messageID newMessage
            case decodeEither $ encodeUtf8 $ body newMessage of
              Left err -> void $ replyMessage newMessage $ Text.pack err
              Right comm -> do
                case comm of
                  MatchCommand mid (NewThread t) ->
                    void $ liftIO $ async $ runR $ postThread key t mid
                  MatchCommand mid (AttachTo p) ->
                    void $ liftIO $ async $ runR $ keepUpdated key mid p
        _ -> return ()
  liftIO $ threadDelay $ 15 * 1000 * 1000
  loop $ foldl (\s a -> Set.insert (messageID a) s) x news

moderators :: Set (Maybe Username)
moderators = Set.fromList $ Just <$> Username <$>
  [ "ReaverXai"
  , "m4rx"
  , "klopjobacid"
  , "Decency"
  , "0Hellspawn0"
  , "wykrhm"
  , "crimson589"
  , "Intolerable"
  , "lestye"
  , "leafeator"
  , "HAPPYSADPERSON"
  , "RawlsTofJ"
  , "MadnessBunny"
  , "coronaria"
  , "772-LR"
  , "Gamerhcp"
  , "aniboy10"
  , "Arch_Reaper"
  , "stats95"
  , "FeeedXD"
  , "jagnev"
  , "gl0ryus" ]

data ThreadCommand = NewThread Text
                   | AttachTo PostID
  deriving (Show, Read, Eq)

data MatchCommand = MatchCommand MatchID ThreadCommand
  deriving (Show, Read, Eq)

instance FromJSON MatchCommand where
  parseJSON (Object o) =
    MatchCommand <$> (MatchID <$> o .: "match_id")
                 <*> (NewThread <$> o .: "title" <|>
                      AttachTo <$> o .: "post_id")
  parseJSON _ = mempty

massAttach :: MonadIO m => [(Integer, Text)] -> RedditT m ()
massAttach xs = do
  forM_ xs $ \(m, p) -> do
    sendMessage (Username "D2TournamentThreads") "matchbot" $
      Lazy.toStrict $ format "match_id: {}\n\npost_id: {}" (m, p)
