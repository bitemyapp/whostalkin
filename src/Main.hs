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


-- data StatusDb = StatusDb {
--   statuses :: Map.Map StatusId ReducedStatus
--   } deriving (Show, Typeable)

type TweetText = T.Text
type Username  = T.Text

-- data ReducedStatus = ReducedStatus {
--     rdsText     :: TweetText
--   , rdsUserName :: Username
--     -- type LanguageCode = String
--   , rdsLang     :: Maybe LanguageCode
--     -- Integer
--   , rdsStatusId :: StatusId
--   } deriving (Show, Typeable)

-- resetStatuses :: Update StatusDb ()
-- resetStatuses = put $ StatusDb Map.empty

-- mergeStatuses :: Map.Map StatusId ReducedStatus -> Update StatusDb ()
-- mergeStatuses m = do
--   StatusDb statuses <- get
--   put $ StatusDb (Map.union statuses m)

-- viewStatuses :: Query StatusDb (Map.Map StatusId ReducedStatus)
-- viewStatuses = do
--   StatusDb statuses <- ask
--   return statuses

-- $(deriveSafeCopy 0 'base ''StatusDb)
-- $(deriveSafeCopy 0 'base ''ReducedStatus)
-- $(makeAcidic ''StatusDb ['mergeStatuses, 'viewStatuses, 'resetStatuses])


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

printStatus :: Status -> IO ()
printStatus status = TIO.putStrLn texty
  where texty = T.concat [ T.pack . show $ status ^. statusId
                         , ": "
                         , status ^. statusUser . userScreenName
                         , ": "
                         , status ^. statusText
                         ]

-- toReducedStatus :: Status -> ReducedStatus
-- toReducedStatus status = rds
--   where rds = ReducedStatus txt user lang sid
--         txt  = status ^. statusText
--         user = status ^. statusUser . userScreenName
--         lang = status ^. statusLang
--         sid  = status ^. statusId

-- Prelude> let exampleStatus = ReducedStatus "Hello, World!" "argumatronic" "EN" 1
-- Prelude> sdb <- openLocalStateFrom "db/" (StatusDb Map.empty)
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN"}]
-- Prelude> Data.Acid.update sdb (AddStatus exampleStatus)
-- ()
-- Prelude> statuses <- query sdb ViewStatuses
-- Prelude> statuses
-- fromList [ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN", rdsStatusId = 1},ReducedStatus {rdsText = "Hello, World!", rdsUserName = "argumatronic", rdsLang = "EN", rdsStatusId = 1}]

-- foldStream :: TWInfo
--            -> Int
--            -> UserParam
--            -> IO (Map.Map StatusId ReducedStatus)
-- foldStream twInfo numTweets user = do
--   withManager $ \mgr -> do
--       sourceWithMaxId twInfo mgr $
--         userTimeline user
--       C.$= CL.isolate numTweets
--       C.$$ CL.foldM reduceStatus Map.empty
--   where reduceStatus m status = return $ Map.insert k rds m
--           where rds = toReducedStatus status
--                 k   = rdsStatusId rds

-- twInfo :: TWInfo
-- twInfo = def
--     { twToken = def { twOAuth = tokens, twCredential = credential }
--     , twProxy = Nothing
--     }

-- dumpStream :: TWInfo -> Manager -> Int -> UserParam -> IO (Map.Map StatusId Status)
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

getFollowers twInfo numTweets user = undefined

twInfo :: TWInfo
twInfo = def {twToken = TWToken {twOAuth = def {oauthServerName = "twitter", oauthRequestUri = "https://api.twitter.com/oauth/request_token", oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token", oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize", oauthSignatureMethod = HMACSHA1, oauthConsumerKey = "qvPuz6IG5ra6GIvT2fy8t1qm6", oauthConsumerSecret = "v23vCkcm3xHZMqqkRwgos4HGiTZa6y0FLndMNZpDhLb8RbDgZF", oauthCallback = Nothing, oauthRealm = Nothing, oauthVersion = OAuth10a}, twCredential = Credential {unCredential = [("oauth_token",""),("oauth_token_secret",""),("user_id","161569820"),("screen_name","bitemyapp"),("x_auth_expires","0")]}}, twProxy = Nothing}

-- main :: IO ()
main = undefined
  -- withManager $ \mgr -> do
  --   m <- dumpStream twInfo mgr 100 (ScreenNameParam "bitemyapp")
  --   print m

    -- twInfo <- getTWInfo
    -- print twInfo
    -- myDb <- openLocalStateFrom "db/" (StatusDb Map.empty)
    -- Data.Acid.update myDb (MergeStatuses m)
    -- Data.Acid.createCheckpoint myDb
    -- Data.Acid.closeAcidState myDb
