{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import West9 (tweetNow, tweetRep, ig, makeOAuthEnv, defaultOAuth)
import System.IO (putStrLn,hFlush,stdout,appendFile)
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.Console.CmdArgs (
  (&=), explicit, name, argPos, cmdArgs, help, Data, Typeable)
import Control.Monad (liftM)
import Control.Exception (IOException,handle,displayException,SomeException)
import Control.Monad.Reader (runReaderT)

data Options = Options {
  optRepID :: Maybe Integer
, optTweetText :: String
} deriving (Show, Eq, Data, Typeable)

defaultOption :: Options
defaultOption = Options {
  optRepID = Nothing &= explicit
    &= name "reply" &= name "r"
    &= help "which tweetID in reply to"
, optTweetText = "" &= argPos 0
}

main ::  IO ()
main = handle ig $ do
  homeDir <- getHomeDirectory
  oauthEnv <- makeOAuthEnv $ homeDir++"/.west9/oauth.config"
  let oauthEnv = defaultOAuth
  opts <- cmdArgs defaultOption
  let tw = optTweetText opts
  case optRepID opts of
    (Just id) -> runReaderT (tweetRep tw id) oauthEnv
    Nothing   -> runReaderT (tweetNow tw   ) oauthEnv
