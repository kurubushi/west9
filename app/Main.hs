module Main where

import West9 (tweetNow, tweetRep, ig, makeOAuthEnv)
import West9Options (Options(..), getOptions)
import System.IO (putStrLn,hFlush,stdout,appendFile)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Exception (IOException,handle,displayException,SomeException)
import Control.Monad.Reader (runReaderT)

main ::  IO ()
main = do
  args <- getArgs
  opts <- getOptions
  let (tw, oauthFilePath) = (optText opts, optConfigFilePath opts)
  oauthEnv <- makeOAuthEnv oauthFilePath
  case optRepID opts of
    (Just id) -> putStrLn $ "tw: " ++ tw ++ "\nid: " ++ (show id)
    Nothing   -> putStrLn $ "tw: " ++ tw
