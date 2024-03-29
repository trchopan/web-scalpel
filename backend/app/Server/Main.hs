import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
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
import           ProductDetail                  ( ProductLink )
import           ProductDetailRepo              ( ProductDetailDTO
                                                  ( ProductDetailDTO
                                                  )
                                                , ProductDetailRow(..)
                                                , ProductDetailRowDTO
                                                  ( ProductDetailRowDTO
                                                  )
                                                , ProductPriceRow(..)
                                                , ProductPriceRowDTO
                                                  ( ProductPriceRowDTO
                                                  )
                                                , queryProductDetails
                                                , queryProductPrices
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
    (fullDesc <> progDesc "my-scalpel-server" <> header "my web scraper server")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.1.0" $ long "version" <> help "Show version"

  programOptions :: Parser Opts
  programOptions = Opts <$> strOption (long "db" <> help "Database file")

groupByKey :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupByKey key as = Map.fromListWith (++) as'
  where as' = map ((,) <$> key <*> (: [])) as

mapper
  :: Map.Map ProductLink [ProductPriceRow]
  -> ProductDetailRow
  -> ProductDetailDTO
mapper pricesMap (ProductDetailRow pLink pName pMoreInfo pImage pSource) =
  ProductDetailDTO (ProductDetailRowDTO pLink pName pMoreInfo pImage pSource)
                   (maybe [] (map mapPriceRow) $ Map.lookup pLink pricesMap)
 where
  mapPriceRow :: ProductPriceRow -> ProductPriceRowDTO
  mapPriceRow (ProductPriceRow _ priceDate priceList priceSpecial) =
    ProductPriceRowDTO priceDate
                       (fromMaybe 0 priceList)
                       (fromMaybe 0 priceSpecial)

getProducts :: Connection -> Opts -> ScottyM ()
getProducts conn (Opts optDbPath) = get "/products" $ do
  products <- liftIO $ queryProductDetails conn
  prices   <- liftIO $ queryProductPrices conn
  let pricesMap = groupByKey (\(ProductPriceRow link _ _ _) -> link) prices

  json $ map (mapper pricesMap) products
