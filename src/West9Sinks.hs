{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module West9Sinks (
  Tweet(..)
, TwitterUser(..)
, watcher
, takeTweetLoop
) where

import qualified Data.Conduit.Binary as CB
import Data.Conduit (($$+-), Sink, ($$++), ($$+), newResumableSource, ResumableSource, await)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Data.Text (Text)
import Control.Exception (IOException,handle,displayException,SomeException)
import Control.Monad ((<=<),forever,liftM,guard,foldM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT,MaybeT(..))
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.IO (putStrLn,hFlush,stdout,appendFile)
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource)
import Data.Aeson (
  json, FromJSON, ToJSON, fromJSON, Result(..),
  parseJSON, withObject, (.:), (.:?))
import GHC.Generics
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

type URL = String
type TweetID = Integer

data Tweet = Tweet {
  id_num :: Integer
, id_str :: Text
, text   :: Text
, user   :: TwitterUser
, retweeted_status :: Maybe Tweet
, in_reply_to_screen_name :: Maybe Text
} deriving (Show, Eq)

instance FromJSON Tweet where
  parseJSON = withObject "tweet" $ \o -> do
    id_num <- o .: "id"
    id_str <- o .: "id_str"
    text <- o .: "text"
    user <- o .: "user"
    retweeted_status <- o .:? "retweeted_status"
    in_reply_to_screen_name <- o .:? "in_reply_to_screen_name"
    return Tweet{..}

data TwitterUser = TwitterUser {
  screen_name :: Text
} deriving (Show, Eq, Generic)
instance FromJSON TwitterUser


watcher :: (MonadIO m, MonadThrow m) => Tweet -> m ()
watcher tw = liftM (fromMaybe ()) . runMaybeT $ do
    let ids = id_str $ tw
    let user' = screen_name . user $ tw
    let text' = text $ tw
    let re' = id_str $ tw
    liftIO $ T.putStrLn ("@"<>user'<>": "<>ids<>"\n"<>text')
    return ()

takeTweetLoop :: (MonadIO m, MonadThrow m) => [Tweet -> m ()] -> Sink BS.ByteString m ()
takeTweetLoop acts = do
  val <- CA.sinkParser json
  case (fromJSON val :: Result Tweet) of
    Success tw -> lift $ mapM_ ($tw) acts
    Error st   -> return ()
  takeTweetLoop acts
