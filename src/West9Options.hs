{-# LANGUAGE RecordWildCards #-}

module West9Options (
  Options(..)
, getOptions
) where

import Options.Applicative
import System.Directory (getHomeDirectory)

data MyEnv = MyEnv {
  askHomeDir :: FilePath
}

makeMyEnv :: IO MyEnv
makeMyEnv = do
  askHomeDir <- getHomeDirectory
  return MyEnv{..}

data Options = PostOptions {
  optRepID :: Maybe Integer
, optConfigFilePath :: FilePath
, optText :: String
} | FilterOptions {
  optConfigFilePath :: FilePath
, optWords :: [String]
} | TimeLineOptions {
  optConfigFilePath :: FilePath
} deriving (Show, Eq)

postOP :: MyEnv -> Parser Options
postOP env = PostOptions
    <$> optional (option auto
      ( long "reply"
     <> short 'r'
     <> metavar "INT"
     <> help "which TweetID in reply to"))
    <*> option auto
      ( long "oauth"
     <> metavar "FILE"
     <> value ((askHomeDir env)++"/.west9/oauth.conf") --default
     <> help "OAuth config file")
    <*> argument str
      ( metavar "TEXT")

filterOP :: MyEnv -> Parser Options
filterOP env = FilterOptions
    <$> option auto
      ( long "oauth"
     <> metavar "FILE"
     <> value ((askHomeDir env)++"/.west9/oauth.conf") --default
     <> help "OAuth config file")
    <*> some (argument str
      ( metavar "WORD..."))

timeLineOP :: MyEnv -> Parser Options
timeLineOP env = TimeLineOptions
    <$> option auto
      ( long "oauth"
     <> metavar "FILE"
     <> value ((askHomeDir env)++"/.west9/oauth.conf") --default
     <> help "OAuth config file")

allOptions :: MyEnv -> Parser Options
allOptions env = subparser $
    command "post" (info (postOP env)
      (progDesc "Post your tweet."))
 <> command "filter" (info (filterOP env)
      (progDesc "Watch tweets filterd."))
 <> command "tl" (info (timeLineOP env)
      (progDesc "Watch your timeline."))

addInfo :: (MyEnv -> Parser Options) -> (MyEnv -> ParserInfo Options)
addInfo parser env = info
  (helper <*> parser env)
  (fullDesc
    <> progDesc "Tweet from W9"
    <> header "Let's tweet from W9!")

getOptions :: IO Options
getOptions = do
  env <- makeMyEnv
  execParser . addInfo allOptions $ env
