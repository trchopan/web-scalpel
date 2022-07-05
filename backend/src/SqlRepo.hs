module SqlRepo where

import           Data.String                    ( IsString(fromString) )
import qualified Data.Text                     as T
import           Database.SQLite.Simple         ( Connection
                                                , FromRow(..)
                                                , Only(Only)
                                                , Query
                                                , execute
                                                , execute_
                                                , field
                                                , query
                                                , query_
                                                )
import           ProductDetail                  ( ProductDetail )
import           ProductDetailRow               ( ProductDetailRow )


-- TODO: Move this to main app opts
tableName :: String
tableName = "scalpel"

getProductDetails :: Connection -> String -> IO [ProductDetailRow]
getProductDetails conn tableName =
  query_ conn (fromString $ "SELECT * from " ++ tableName)

initDB :: Connection -> String -> IO ()
initDB conn tableName =
  execute_ conn
    $  fromString
    $  unwords
    $  ["CREATE TABLE IF NOT EXISTS ", tableName]
    <> [ "("
       , "  id INTEGER PRIMARY KEY"
       , ", name TEXT"
       , ", link TEXT"
       , ", moreInfo TEXT"
       , ", image TEXT"
       , ", priceList INT"
       , ", priceSpecial INT"
       , ", source TEXT"
       , ", date DATE"
       , ")"
       ]

insertFields :: String -> [String] -> Query
insertFields tableName fields =
  fromString
    $  unwords
    $  ["INSERT INTO", tableName]
    <> ["(", commaSeparatedFields, ") VALUES (", questionSeparatedFields, ")"]
 where
  unpackAndInterlate      = T.unpack . T.intercalate ","
  commaSeparatedFields    = (unpackAndInterlate . map T.pack) fields
  questionSeparatedFields = (unpackAndInterlate . map (const "?")) fields

insertProductDetail :: Connection -> String -> [ProductDetail] -> IO ()
insertProductDetail conn tableName = mapM_
  (execute conn $ insertFields
    tableName
    [ "name"
    , "link"
    , "moreInfo"
    , "image"
    , "priceList"
    , "priceSpecial"
    , "source"
    , "date"
    ]
  )

deleteByDate :: Connection -> String -> String -> IO ()
deleteByDate conn tableName date = execute conn myQuery (Only date)
  where myQuery = fromString $ unwords ["DELETE FROM", tableName, "WHERE date=?"]

checkExistDate :: Connection -> String -> String -> IO Bool
checkExistDate conn tableName date =
  not . null <$> (query conn myQuery (Only date) :: IO [T.Text])
 where
  myQuery =
    fromString $ unwords ["SELECT name FROM", tableName, "WHERE date=? LIMIT 1"]

instance FromRow T.Text where
  fromRow = field
