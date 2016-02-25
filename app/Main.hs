{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import West9 (tweetNow, tweetRep, ig, makeOAuthEnv)
import System.IO (putStrLn,hFlush,stdout,appendFile)
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.Console.GetOpt
import Control.Monad (liftM)
import Control.Exception (IOException,handle,displayException,SomeException)
import Control.Monad.Reader (runReaderT)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

data Options = Options {
  optRepID :: Maybe Integer
, optConfigFilePath :: FilePath
} deriving (Show, Eq)

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['r'] ["reply"]
      (OptArg (\mst opts -> opts {optRepID=liftM read mst}) "TweetID")
      "which TweetID in reply to"
  , Option [] ["oauth"]
      (OptArg ( (\fp opts -> opts {optConfigFilePath=fp}) . fromMaybe "$HOME/.west9/oauth.conf")
        "FILE")
      "OAuth config file path"
  ]

defaultOpts :: Options
defaultOpts = Options {
  optRepID = Nothing
, optConfigFilePath = "$HOME/.west9/oauth.conf"
}

main ::  IO ()
main = handle ig $ do
  args <- getArgs
  (opts, tw) <- case getOpt Permute options args of
    (optf, [rest], []) -> return (foldl' (flip id) defaultOpts optf, rest)
    (_,_,error       ) -> fail $ show error
  let oauthFilePath = optConfigFilePath opts
  oauthEnv <- makeOAuthEnv oauthFilePath
  case optRepID opts of
    (Just id) -> runReaderT (tweetRep tw id) oauthEnv
    Nothing   -> runReaderT (tweetNow tw   ) oauthEnv
