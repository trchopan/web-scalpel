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
import           Opts                           ( Command(OutputCmd, PersistCmd)
                                                , OutputOpt(OutputOpt)
                                                , PersistOpt(PersistOpt)
                                                , parseOpts
                                                )
import           ProductDetail                  ( ProductDetail
                                                , ProductSource
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
import           Scrapers                       ( scraperForSource )
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
main = parseOpts >>= processOpts

processOpts :: Command -> IO ()
processOpts (OutputCmd  opts) = outputHandler opts
processOpts (PersistCmd opts) = persistHandler opts


outputHandler :: OutputOpt -> IO ()
outputHandler (OutputOpt inputPath source date) =
  openFileAndScrape inputPath source date >>= print

persistHandler :: PersistOpt -> IO ()
persistHandler opts = do
  configs <- loadConfigs optConfigPath
  dirs    <- listDirectory optDataPath
  let dateReg       = mkRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}"
      dateFolders   = filter (isJust . matchRegex dateReg) dirs
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
  where (PersistOpt optConfigPath optDataPath optDb optForce) = opts


scrapeFolder :: Connection -> PersistOpt -> [Config] -> String -> IO ()
scrapeFolder conn opts configs folder = do
  putStrLn $ "Scraping " ++ folder
  allProducts <- mapM (openDataPathFileAndScrape opts folder) configs

  let products                          = foldl (<>) [] allProducts
      productRows :: [ProductDetailRow] = map fromProductDetail products
      priceRows :: [ProductPriceRow]    = map fromProductDetail products

  upsertProductDetail conn productRows
  insertPrice conn priceRows

  putStrLn $ printf "Found %d products" (length products)
  where (PersistOpt optConfigPath optDataPath optDb optForce) = opts


openDataPathFileAndScrape
  :: PersistOpt -> String -> Config -> IO [ProductDetail]
openDataPathFileAndScrape (PersistOpt _ optDataPath _ _) date (Config _ source name)
  = openFileAndScrape scrapePath source date
  where scrapePath = optDataPath </> date </> name <.> "html"


openFileAndScrape :: FilePath -> ProductSource -> String -> IO [ProductDetail]
openFileAndScrape scrapePath source date =
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


tryOpenFile :: FilePath -> IO (Maybe Handle)
tryOpenFile path =
  handle (\(e :: IOException) -> print e >> return Nothing) $ do
    h <- openFile path ReadMode
    return (Just h)

exitUnknownSource :: IO a
exitUnknownSource = exitWithErrorMessage "Unknown Source" (ExitFailure 1)

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e
