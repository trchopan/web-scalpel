{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Database.SQLite.Simple         ( Connection
                                                , close
                                                , open
                                                )
import           Network.Wai.Middleware.Cors    ( simpleCors )
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
                                                )
import           ProductDetailRow               ( ProductDetailRow
                                                  ( ProductDetailRow
                                                  )
                                                )
import           SqlRepo                        ( getProductDetails
                                                , tableName
                                                )
import           Web.Scotty                     ( ScottyM
                                                , get
                                                , json
                                                , middleware
                                                , scotty
                                                )


main :: IO ()
main = do
  opts <- parseOpts
  conn <- open (optDbPath opts)

  scotty 3000 $ middleware simpleCors >> getProducts conn opts

  close conn

newtype Opts = Opts
  { optDbPath :: FilePath
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
  programOptions = Opts <$> strOption (long "db" <> help "Database file")

getProducts :: Connection -> Opts -> ScottyM ()
getProducts conn (Opts optDbPath) = get "/products" $ do
  rows <- liftIO $ getProductDetails conn tableName
  let products = map (\(ProductDetailRow _ pd) -> pd) rows
  json products
