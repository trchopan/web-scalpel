{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Config                         ( Config(Config)
                                                , loadConfigs
                                                )
import           Control.Exception              ( IOException
                                                , handle
                                                )
import           Control.Monad                  ( filterM
                                                , when
                                                )
import           Data.Maybe                     ( isJust )
import           Data.String                    ( IsString(fromString) )
import           Database.SQLite.Simple         ( Connection
                                                , open
                                                )
import           Opts                           ( Opts(Opts)
                                                , parseOpts
                                                )
import           ProductDetail                  ( ProductDetail
                                                , scraperForSource
                                                )
import           ProductDetailRepo              ( ProductDetailRow
                                                , ProductPriceRow
                                                , checkPricesExistDate
                                                , deletePricesByDate
                                                , fromProductDetail
                                                , initPriceTable
                                                , initProductTable
                                                , insertPrice
                                                , upsertProductDetail
                                                )
import           System.Directory               ( listDirectory )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO                      ( Handle
                                                , IOMode(ReadMode)
                                                , hGetContents
                                                , hPutStrLn
                                                , openFile
                                                , stderr
                                                )
import           Text.Printf                    ( printf )
import           Text.Regex                     ( matchRegex
                                                , mkRegex
                                                )


main :: IO ()
main = do
  opts <- parseOpts
  loadConfigsWithOpts opts

loadConfigsWithOpts :: Opts -> IO ()
loadConfigsWithOpts opts = do
  configs <- loadConfigs optConfigPath
  dirs    <- listDirectory optDataPath
  let reg           = mkRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}"
      dateFolders   = filter (isJust . matchRegex reg) dirs
      outputFolders = map (optDataPath </>) dateFolders

  putStrLn $ "Found folders: " ++ show dateFolders

  conn <- open optDb
  initProductTable conn
  initPriceTable conn
  existFolders <- filterM (checkPricesExistDate conn) dateFolders
  putStrLn $ "Exist folders: " ++ show existFolders

  when optForce $ mapM_ (deletePricesByDate conn) existFolders

  let tobeScraped = if optForce
        then dateFolders
        else filter (`notElem` existFolders) dateFolders

  putStrLn $ "Scrape folders: " ++ show tobeScraped
  mapM_ (scrapeFolder conn opts configs) tobeScraped
  where (Opts optConfigPath optDataPath optDb optForce) = opts


scrapeFolder :: Connection -> Opts -> [Config] -> String -> IO ()
scrapeFolder conn opts configs folder = do
  putStrLn $ "Scraping " ++ folder
  allProducts <- mapM (openFileAndScrape opts folder) configs

  let products                          = foldl (<>) [] allProducts
      productRows :: [ProductDetailRow] = map fromProductDetail products
      priceRows :: [ProductPriceRow]    = map fromProductDetail products

  upsertProductDetail conn productRows
  insertPrice conn priceRows

  putStrLn $ printf "Found %d products" (length products)
  where (Opts optConfigPath optDataPath optDb optForce) = opts

openFileAndScrape :: Opts -> String -> Config -> IO [ProductDetail]
openFileAndScrape (Opts _ optDataPath _ optForce) date (Config _ source name) =
  tryOpenFile scrapePath
    >>= (\case
          Nothing -> do
            putStrLn $ printf "Skip %s." scrapePath
            return ""
          Just io -> hGetContents io
        )
    >>= (\content -> case scraperForSource source of
          Nothing      -> exitUnknownSource
          Just scraper -> scraper content (fromString date)
        )
    >>= \case
          Nothing  -> pure []
          Just pds -> pure pds
  where scrapePath = optDataPath </> date </> name <.> "html"


tryOpenFile :: FilePath -> IO (Maybe Handle)
tryOpenFile path =
  handle (\(e :: IOException) -> print e >> return Nothing) $ do
    h <- openFile path ReadMode
    return (Just h)


exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

exitUnknownSource :: IO a
exitUnknownSource = exitWithErrorMessage "Unknown Source" (ExitFailure 1)
