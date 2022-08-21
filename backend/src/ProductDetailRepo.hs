{-# LANGUAGE DeriveGeneric #-}

module ProductDetailRepo where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decodeStrict
                                                , encode
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( IsString(fromString) )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LBS
import           Database.SQLite.Simple         ( FromRow(..)
                                                , Only(Only)
                                                , Query
                                                , ResultError(ConversionFailed)
                                                , SQLData(SQLInteger, SQLText)
                                                , ToRow(..)
                                                , execute
                                                , execute_
                                                , field
                                                , query
                                                , query_
                                                )
import           Database.SQLite.Simple.FromField
                                                ( FromField(..)
                                                , returnError
                                                )
import           Database.SQLite.Simple.Internal
                                                ( Connection
                                                , Field(Field)
                                                , RowParser
                                                )
import           Database.SQLite.Simple.Ok      ( Ok(Ok) )
import           GHC.Generics                   ( Generic )
import           ProductDetail                  ( ProductDate
                                                , ProductDetail(ProductDetail)
                                                , ProductImage
                                                , ProductLink
                                                , ProductMoreInfo(..)
                                                , ProductName
                                                , ProductSource(UnknownSource)
                                                )
import qualified ProductDetail


encoder :: ToJSON a => a -> T.Text
encoder = LT.toStrict . LBS.decodeUtf8 . encode

decoder :: FromJSON a => T.Text -> Maybe a
decoder = decodeStrict . encodeUtf8

instance FromField ProductMoreInfo where
  fromField (Field (SQLText txt) _) =
    Ok $ fromMaybe (ProductMoreInfo []) (decoder txt)
  fromField f = returnError ConversionFailed f "expecting product moreInfo"

data ProductDetailRow = ProductDetailRow ProductLink
                                         ProductName
                                         ProductMoreInfo
                                         ProductImage
                                         ProductSource
  deriving Show


data ProductDetailRowDTO = ProductDetailRowDTO
  { link     :: ProductLink
  , name     :: ProductName
  , moreInfo :: ProductMoreInfo
  , image    :: ProductImage
  , source   :: ProductSource
  }
  deriving (Show, Generic)

instance ToJSON ProductDetailRowDTO


instance FromField ProductSource where
  fromField (Field (SQLText txt) _) =
    Ok $ fromMaybe UnknownSource (decoder txt)
  fromField f = returnError ConversionFailed f "expecting product source"


instance FromRow ProductDetailRow where
  fromRow =
    ProductDetailRow
      <$> (field :: RowParser ProductLink)
      <*> (field :: RowParser ProductName)
      <*> (field :: RowParser ProductMoreInfo)
      <*> (field :: RowParser ProductImage)
      <*> (field :: RowParser ProductSource)


instance ToRow ProductDetailRow where
  toRow (ProductDetailRow pLink pName pMoreInfo pImage pSource) =
    [ SQLText pLink
    , SQLText pName
    , SQLText $ encoder pMoreInfo
    , SQLText pImage
    , SQLText $ encoder pSource
    ]

data ProductPriceRow = ProductPriceRow ProductLink
                                       ProductDate
                                       (Maybe Integer)
                                       (Maybe Integer)
  deriving Show


instance FromRow ProductPriceRow where
  fromRow =
    ProductPriceRow
      <$> (field :: RowParser ProductLink)
      <*> (field :: RowParser ProductDate)
      <*> (field :: RowParser (Maybe Integer))
      <*> (field :: RowParser (Maybe Integer))


data ProductPriceRowDTO = ProductPriceRowDTO
  { date    :: ProductDate
  , list    :: Integer
  , special :: Integer
  }
  deriving (Show, Generic)

instance ToJSON ProductPriceRowDTO

instance ToRow ProductPriceRow where
  toRow (ProductPriceRow pLink pDate pList pSpecial) =
    [ SQLText pLink
    , SQLText pDate
    , SQLInteger $ fromIntegral $ fromMaybe 0 pList
    , SQLInteger $ fromIntegral $ fromMaybe 0 pSpecial
    ]


class FromProductDetail a where
  fromProductDetail :: ProductDetail -> a

instance FromProductDetail ProductDetailRow where
  fromProductDetail (ProductDetail name link image moreInfo price source date)
    = ProductDetailRow link name moreInfo image source

instance FromProductDetail ProductPriceRow where
  fromProductDetail (ProductDetail name link image moreInfo price source date)
    = ProductPriceRow link
                      date
                      (ProductDetail.list price)
                      (ProductDetail.special price)


queryProductDetails :: Connection -> IO [ProductDetailRow]
queryProductDetails conn = query_ conn "SELECT * FROM products"


initProductTable :: Connection -> IO ()
initProductTable conn =
  execute_ conn
    $  fromString
    $  unwords
    $  ["CREATE TABLE IF NOT EXISTS products"]
    <> [ "("
       , "  link TEXT PRIMARY KEY"
       , ", name TEXT"
       , ", moreInfo TEXT"
       , ", image TEXT"
       , ", source TEXT"
       , ")"
       ]


commaSeparate :: [T.Text] -> String
commaSeparate = T.unpack . T.intercalate ","


insertFields :: String -> [String] -> Query
insertFields tableName fields =
  fromString
    $  unwords
    $  ["INSERT INTO", tableName]
    <> ["(", fieldsWithComma, ") VALUES (", questionsFields, ")"]
 where
  fieldsWithComma = commaSeparate . map T.pack $ fields
  questionsFields = commaSeparate . map (const "?") $ fields


upsertProductDetail :: Connection -> [ProductDetailRow] -> IO ()
upsertProductDetail conn = mapM_
  (execute conn $ fromString $ unwords
    [ "INSERT INTO products (" ++ commaSeparate fields ++ ")"
    , "VALUES("
    , (commaSeparate . map (const "?")) fields
    , ")"
    , "ON CONFLICT(link)"
    , "DO UPDATE SET "
    , (commaSeparate . map (\f -> f <> "=excluded." <> f)) fields
    ]
  )
  where fields = ["link", "name", "moreInfo", "image", "source"]


queryProductPrices :: Connection -> IO [ProductPriceRow]
queryProductPrices conn = query_ conn "SELECT * FROM prices"


initPriceTable :: Connection -> IO ()
initPriceTable conn =
  execute_ conn
    $  fromString
    $  unwords
    $  ["CREATE TABLE IF NOT EXISTS prices"]
    <> [ "("
       , "  link TEXT"
       , ", date TEXT KEY"
       , ", priceList INT"
       , ", priceSpecial INT"
       , ", FOREIGN KEY(link) REFERENCES products(link)"
       , ")"
       ]


insertPrice :: Connection -> [ProductPriceRow] -> IO ()
insertPrice conn = mapM_
  ( execute conn
  $ insertFields "prices" ["link", "date", "priceList", "priceSpecial"]
  )


deletePricesByDate :: Connection -> String -> IO ()
deletePricesByDate conn date =
  execute conn "DELETE FROM prices WHERE date=?" (Only date)


checkPricesExistDate :: Connection -> String -> IO Bool
checkPricesExistDate conn date =
  not . null <$> (query conn myQuery (Only date) :: IO [T.Text])
  where myQuery = fromString "SELECT date FROM prices WHERE date=? LIMIT 1"


instance FromRow T.Text where
  fromRow = field

data ProductDetailDTO = ProductDetailDTO
  { detail :: ProductDetailRowDTO
  , prices :: [ProductPriceRowDTO]
  }
  deriving (Show, Generic)

instance ToJSON ProductDetailDTO


equalPriceLinks :: ProductPriceRow -> ProductPriceRow -> Bool
equalPriceLinks (ProductPriceRow pLink1 _ _ _) (ProductPriceRow pLink2 _ _ _) =
  pLink1 == pLink2


comparePriceLinks :: ProductPriceRow -> ProductPriceRow -> Ordering
comparePriceLinks (ProductPriceRow pLink1 _ _ _) (ProductPriceRow pLink2 _ _ _)
  = compare pLink1 pLink2

