{-# LANGUAGE RecordWildCards #-}

module Main where

import West9 (tweetNow, tweetRep, ig, makeOAuthEnv, filterWatch, timeLineWatch, getEndNotifyEnv, endNotify)
import West9Options (Options(..), getOptions)
import Control.Monad.Reader (runReaderT)

exec :: Options -> IO ()
exec (PostOptions {..}) = do
  let (tw, oauthFilePath) = (optText, optConfigFilePath)
  oauthEnv <- makeOAuthEnv oauthFilePath
  case optRepID of
    (Just id) -> runReaderT (tweetRep tw id) oauthEnv
    Nothing   -> runReaderT (tweetNow tw   ) oauthEnv
exec (FilterOptions {..}) = do
  let (sts, oauthFilePath) = (optWords, optConfigFilePath)
  oauthEnv <- makeOAuthEnv oauthFilePath
  case sts of
    (_:_) -> runReaderT (filterWatch sts) oauthEnv
    []    -> return ()
exec (TimeLineOptions {..}) = do
  let (tw, oauthFilePath) = (optText, optConfigFilePath)
  oauthEnv <- makeOAuthEnv oauthFilePath
  runReaderT timeLineWatch oauthEnv
exec (EndNotifyOptions {..}) = do
  let (oauthFilePath, endNotifyFilePath) = (optConfigFilePath, optEndNotifyFilePath)
  oauthEnv <- makeOAuthEnv oauthFilePath
  endNotifyEnv <- getEndNotifyEnv endNotifyFilePath
  runReaderT (endNotify endNotifyEnv) oauthEnv

main ::  IO ()
main = do
  opts <- getOptions
  exec opts
