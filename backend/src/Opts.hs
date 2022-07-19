module Opts where

import           Options.Applicative            ( CommandFields
                                                , Mod
                                                , Parser
                                                , ParserInfo
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , hsubparser
                                                , info
                                                , infoOption
                                                , long
                                                , progDesc
                                                , strOption
                                                , switch
                                                )
import           ProductDetail                  ( ProductSource )


data OutputOpt = OutputOpt
  { outputInputPath :: FilePath
  , outputSource    :: ProductSource
  , outputDate      :: String
  }
  deriving Show

data PersistOpt = PersistOpt
  { persistConfigPath :: FilePath
  , persistDataPath   :: FilePath
  , persistDbPath     :: FilePath
  , persistForce      :: Bool
  }
  deriving Show

data Command
    = OutputCmd OutputOpt
    | PersistCmd PersistOpt

parseOpts :: IO Command
parseOpts = execParser optsParser
 where
  optsParser :: ParserInfo Command
  optsParser = info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "my-scalpel" <> header "my web scraper")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.1.0" $ long "version" <> help "Show version"

  programOptions :: Parser Command
  programOptions = hsubparser (persistCmd <> outputCmd)

  persistCmd :: Mod CommandFields Command
  persistCmd = command "persist" $ info
    (   PersistCmd
    <$> (   PersistOpt
        <$> strOption
              (long "config" <> help "Config file path. eg: ./config.yaml")
        <*> strOption (long "data" <> help "Data file path. eg: ../data")
        <*> strOption
              (long "db" <> help "Database file path. eg: scalpel.db")
        <*> switch (long "force" <> help "Force replace existing data")
        )
    )
    (progDesc "Persist the data into sqlite3 database")

  outputCmd :: Mod CommandFields Command
  outputCmd = command "output" $ info
    (   OutputCmd
    <$> (   OutputOpt --
        <$> strOption (long "input" <> help "Data file path. eg: ../data")
        <*> strOption
              (long "source" <> help "Source type. eg: cellphones.com.vn")
        <*> strOption
              (long "date" <> help "Date of the scrape. eg: 2022-07-19")
        )
    )
    (progDesc "Output to terminal. Use for debuging.")
