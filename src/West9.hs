{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module West9 (
  OAuthEnv
, makeOAuthEnv
, parseUrlPost
, tweetGen
, tweetNow
, tweetRep
, filterWatch
, timeLineWatch
, ig
) where

import West9Sinks (watcher, takeTweetLoop)
import Web.Authenticate.OAuth (signOAuth, OAuth(..), Credential(..), newOAuth, newCredential)
import Data.Text (Text)
import Network.HTTP.Conduit (
  Request(..), urlEncodedBody, parseUrl, newManager, tlsManagerSettings, http, responseBody)
import Control.Exception (IOException,handle,displayException,SomeException)
import Control.Monad ((<=<),forever,liftM,guard,foldM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT,MaybeT(..))
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Conduit.Binary as CB
import Data.Conduit (($$+-), Sink, ($$++), ($$+), newResumableSource, ResumableSource, await)
import qualified Data.Conduit.List as CL
import System.IO (putStrLn,hFlush,stdout,appendFile)
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource)
import Data.Aeson (
  json, FromJSON, ToJSON, fromJSON, Result(..),
  parseJSON, withObject, (.:), (.:?))
import GHC.Generics
import Data.Monoid ((<>))
import System.Random (randomRIO)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Codec.Binary.UTF8.String as UTF8
import Data.ConfigFile (readfile, emptyCP, get, ConfigParser(..))
import Data.Either.Utils (forceEither)

type URL = String
type TweetID = Integer
data OAuthEnv = OAuthEnv {
  getOAuth :: OAuth
, getCredential :: Credential
}

makeOAuthEnv :: FilePath -> IO OAuthEnv
makeOAuthEnv file = do
  val <- readfile emptyCP{optionxform = id} file
  let cp = forceEither val
  let oauthCK = forceEither $ get cp "OAuth" "ConsumerKey"
  let oauthCS = forceEither $ get cp "OAuth" "ConsumerSecret"
  let credentialAT = forceEither $ get cp "Credential" "AccessToken"
  let credentialATS = forceEither $ get cp "Credential" "AccessTokenSecret"
  let getOAuth = newOAuth {
    oauthRequestUri = "https://api.twitter.com/oauth/request_token",
    oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token",
    oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize",
    oauthConsumerKey = BS8.pack oauthCK,
    oauthConsumerSecret = BS8.pack oauthCS}
  let getCredential = newCredential (BS8.pack credentialAT) (BS8.pack credentialATS)
  return OAuthEnv {..}

parseUrlPost :: MonadThrow m => [(BS.ByteString,BS.ByteString)] -> String -> m Request
parseUrlPost post = return . urlEncodedBody post <=< parseUrl

tweetGen :: (MonadIO m, MonadThrow m, MonadResource m, MonadReader OAuthEnv m) =>
  Request -> Sink BS.ByteString m o -> m o
tweetGen req sink = do
  oauth <- liftM getOAuth $ ask
  credential <- liftM getCredential $ ask
  manager <- liftIO $ newManager tlsManagerSettings
  signedRequest <- signOAuth oauth credential req
  res <- http signedRequest manager
  responseBody res $$+- sink

tweetNow :: String -> ReaderT OAuthEnv IO ()
tweetNow st = runResourceT $ do
  req <- liftIO $ parseUrlPost
    [("status", BS8.pack . UTF8.encodeString $ st)]
    "https://api.twitter.com/1.1/statuses/update.json"
  tweetGen req CL.sinkNull
  return ()

tweetRep :: String -> TweetID -> ReaderT OAuthEnv IO ()
tweetRep st toReply = runResourceT $ do
  req <- liftIO $ parseUrlPost
    [("status", BS8.pack . UTF8.encodeString $ st), ("in_reply_to_status_id", BS8.pack . show $ toReply)]
    "https://api.twitter.com/1.1/statuses/update.json"
  tweetGen req CL.sinkNull
  return ()

flushingFunc :: MonadIO m => (a -> m b) -> (a -> m ())
flushingFunc = ((>> liftIO (hFlush stdout)) .)

filterWatch :: [String] -> ReaderT OAuthEnv IO ()
filterWatch sts = runResourceT $ do
  req <- liftIO $ parseUrl . addFilterWords sts $
    "https://stream.twitter.com/1.1/statuses/filter.json" 
  tweetGen req . takeTweetLoop . map flushingFunc $ [watcher]

addFilterWords :: [String] -> URL -> URL
addFilterWords sts url = url ++ "?track=" ++ sts'
  where sts' = L.intercalate "," sts

timeLineWatch :: ReaderT OAuthEnv IO ()
timeLineWatch = runResourceT $ do
  req <- liftIO $ parseUrl
    "https://userstream.twitter.com/1.1/user.json"
  tweetGen req . takeTweetLoop . map flushingFunc $ [watcher]

ig :: SomeException -> IO ()
ig = putStrLn . displayException
