module West9Options (
  Options(..)
, getOptions
) where

import Options.Applicative
import System.Directory (getHomeDirectory)

data Options = Options {
  optRepID :: Maybe Integer
, optConfigFilePath :: FilePath
, optText :: String
} deriving (Show, Eq)

argsParser :: IO (Parser Options)
argsParser = do
  homeDir <- getHomeDirectory
  return $ Options
    <$> optional (option auto
      ( long "reply"
     <> short 'r'
     <> metavar "INT"
     <> help "which TweetID in reply to"))
    <*> option auto
      ( long "oauth"
     <> metavar "FILE"
     <> value (homeDir++"/.west9/oauth.conf") --default
     <> help "OAuth config file")
    <*> argument str
      ( metavar "TEXT")

argsParserInfo :: IO (ParserInfo Options)
argsParserInfo = do
  argsParser' <- argsParser
  return $ info (helper <*> argsParser')
    (fullDesc
      <> progDesc "Tweet from W9"
      <> header "Let's tweet from W9!")

getOptions :: IO Options
getOptions = argsParserInfo >>= execParser
