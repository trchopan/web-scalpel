module Opts where

import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , infoOption
                                                , long
                                                , progDesc
                                                , strOption
                                                , switch
                                                )


data Opts = Opts
  { optConfigPath :: FilePath
  , optDataPath   :: FilePath
  , optDbPath     :: FilePath
  , optForce      :: Bool
  }
  deriving Show

parseOpts :: IO Opts
parseOpts = execParser optsParser
 where
  optsParser :: ParserInfo Opts
  optsParser = info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "scraper" <> header "my web scraper")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.1.0" $ long "version" <> help "Show version"

  programOptions :: Parser Opts
  programOptions =
    Opts --
      <$> strOption
            (long "config" <> help "Config file path. eg: ./config.yaml")
      <*> strOption (long "data" <> help "Data file path. eg: ../data")
      <*> strOption (long "db" <> help "Database file path. eg: scalpel.db")
      <*> switch (long "force" <> help "Force replace existing data")
