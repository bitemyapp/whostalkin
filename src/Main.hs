{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- import           Control.Monad.Trans.Control
-- import           Data.Acquire
import           Control.Applicative ((<$>))
import           Control.Lens hiding ((|>))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)
import           Control.Monad.Trans.Control (MonadBaseControl)
-- import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Trans.Resource
-- import           Data.Acid
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map
-- import           Data.SafeCopy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Typeable (Typeable)
import           Network.HTTP.Conduit
import           System.Environment (getEnv)
import           System.IO (hFlush, stdout)
import           Web.Authenticate.OAuth hiding (insert)
import qualified Web.Authenticate.OAuth as OA
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

authorize :: (MonadBaseControl IO m, MonadResource m) =>
                OA.OAuth -- ^ OAuth Consumer key and secret
             -> (String -> m String) -- ^ PIN prompt
             -> Manager
             -> m OA.Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    getAccessToken oauth
      (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

getTWInfo :: IO TWInfo
getTWInfo = do
  key <- getEnv "OAUTH_KEY"
  secret <- getEnv "OAUTH_SECRET"
  let tokens = twitterOAuth {
          oauthConsumerKey = B8.pack key
        , oauthConsumerSecret = B8.pack secret
        }
  cred <- withManager $ \mgr -> authorize tokens getPIN mgr
  return $ setCredential tokens cred def
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

twInfo :: TWInfo
twInfo = def {twToken = TWToken {twOAuth = def {oauthServerName = "twitter", oauthRequestUri = "https://api.twitter.com/oauth/request_token", oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token", oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize", oauthSignatureMethod = HMACSHA1, oauthConsumerKey = "qvPuz6IG5ra6GIvT2fy8t1qm6", oauthConsumerSecret = "v23vCkcm3xHZMqqkRwgos4HGiTZa6y0FLndMNZpDhLb8RbDgZF", oauthCallback = Nothing, oauthRealm = Nothing, oauthVersion = OAuth10a}, twCredential = Credential {unCredential = [("oauth_token",""),("oauth_token_secret",""),("user_id","161569820"),("screen_name","bitemyapp"),("x_auth_expires","0")]}}, twProxy = Nothing}

dumpStream :: MonadResource m => TWInfo -> Manager
              -> Int -> UserParam -> m (Map.Map StatusId Status)
dumpStream twInfo mgr numTweets user = do
    sourceWithMaxId twInfo mgr $
      userTimeline user
    C.$= CL.isolate numTweets
    C.$$ CL.foldM insertStatuses Map.empty
  where insertStatuses m status =
          return $ Map.insert k status m
         where k = status ^. statusId

-- getFriends :: MonadResource m =>
--               TWInfo -> Int -> UserParam -> m [UserId]
-- getFriends twInfo mgr numTweets user = do
--   friendsIds user $

-- following?, name, screen name
type SummaryUser = (UserId, T.Text, T.Text)

summarize :: User -> SummaryUser
summarize u = (u ^. userId, u ^. userName, u ^. userScreenName)

findConversations :: T.Text -> T.Text
                  -> APIRequest SearchTweets (SearchResult [SearchStatus])
findConversations me friend = searchTweets query
  where query = T.concat ["from:", friend, " @", me]

scoreRelationship :: SearchResult [SearchStatus] -> Integer
scoreRelationship = undefined

-- from:sjfloat @bitemyapp

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    -- let swc = sourceWithCursor twInfo mgr
        -- c = call twInfo mgr
    -- friends <- c (friendsIds (ScreenNameParam "bitemyapp") & count ?~ 5)
    -- let cond = swc $ friendsIds (ScreenNameParam "bitemyapp")
    -- cond C.$= CL.isolate 5
    --      C.=$= CL.map (usersShow . UserIdParam)
    --      -- `C.fuseBoth` CL.mapM (sourceWithCursor twInfo mgr)
    --      C.$$ CL.mapM_ (liftIO . print)
    -- let friendProfiles = fmap usersShow
    -- friends' <-  fmap (\f -> call twInfo mgr (usersShow (UserIdParam f))) friends
    -- let source = swc $ friendsIds (ScreenNameParam "bitemyapp")
    --     profiles = source C.$= CL.isolate 5 C.$= CL.mapM (usersShow . UserIdParam)
    friendsCursor <- call twInfo mgr (friendsList (ScreenNameParam "bitemyapp") & count ?~ 5)
    let friends = fmap summarize (contents friendsCursor)
    --     profilesFetch = fmap (usersShow . UserIdParam) friendIds
    -- profiles <- mapM (call twInfo mgr) profilesFetch
    liftIO $ print friends
    return ()
    -- C.$$ liftIO . print
    -- let friendProfiles = C.$$ CL.mapM (usersShow . UserIdParam)
    -- liftIO $ print friendProfiles
  -- where getProfile twInfo mgr f = call twInfo mgr (usersShow (UserIdParam f))
    -- m <- dumpStream twInfo mgr 25 (ScreenNameParam "bitemyapp")
    -- liftIO $ print m
