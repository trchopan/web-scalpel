module Main where

import           Config                         ( Config(Config)
                                                , loadConfigs
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
                                                , Source
                                                  ( CellphonesVN
                                                  , FptShop
                                                  , TheGioiDiDong
                                                  , UnknownSource
                                                  )
                                                , cellphoneScraper
                                                , fptScraper
                                                , tgddScraper
                                                )
import           SqlRepo                        ( checkExistDate
                                                , deleteByDate
                                                , initDB
                                                , insertProductDetail
                                                , tableName
                                                )
import           System.Directory               ( listDirectory )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO                      ( IOMode(ReadMode)
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
  initDB conn tableName
  existFolders <- filterM (checkExistDate conn tableName) dateFolders
  putStrLn $ "Exist folders: " ++ show existFolders
  when optForce $ mapM_ (deleteByDate conn tableName) existFolders

  let tobeScaped = if optForce
        then dateFolders
        else filter (`notElem` existFolders) dateFolders

  putStrLn $ "Scrape folders: " ++ show tobeScaped
  mapM_ (scapeNewFolder conn opts configs) tobeScaped
  where (Opts optConfigPath optDataPath optDb optForce) = opts

scapeNewFolder :: Connection -> Opts -> [Config] -> String -> IO ()
scapeNewFolder conn opts configs folder = do
  putStrLn $ "Scraping " ++ folder
  allProducts <- mapM (openFileAndScrape opts folder) configs
  let products = foldl (<>) [] allProducts

  insertProductDetail conn tableName products
  putStrLn $ printf "Found %d products" (length products)
  where (Opts optConfigPath optDataPath optDb optForce) = opts

openFileAndScrape :: Opts -> String -> Config -> IO [ProductDetail]
openFileAndScrape (Opts _ optDataPath _ optForce) date (Config _ source name) =
  openFile (scrapePath <.> "html") ReadMode
    >>= hGetContents
    >>= (\content -> case source of
          CellphonesVN  -> cellphoneScraper content (fromString date)
          TheGioiDiDong -> tgddScraper content (fromString date)
          FptShop       -> fptScraper content (fromString date)
          UnknownSource -> exitUnknownSource
        )
    >>= \case
          Nothing  -> pure []
          Just pds -> pure pds
  where scrapePath = optDataPath </> date </> name

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

exitUnknownSource :: IO a
exitUnknownSource = exitWithErrorMessage "Unknown Source" (ExitFailure 1)
