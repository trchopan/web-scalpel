{-# LANGUAGE DeriveGeneric #-}
module ProductDetailRow where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON(toJSON)
                                                , decodeStrict
                                                , encode
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LBS
import           Database.SQLite.Simple         ( FromRow(..)
                                                , SQLData(SQLInteger, SQLText)
                                                , ToRow(..)
                                                , field
                                                )
import           Database.SQLite.Simple.FromRow ( RowParser )
import           GHC.Generics                   ( Generic )
import           ProductDetail                  ( ProductDetail(..)
                                                , ProductPrice
                                                  ( ProductPrice
                                                  , list
                                                  , special
                                                  )
                                                , Source(UnknownSource)
                                                )

data ProductDetailRow = ProductDetailRow Int ProductDetail
  deriving (Show, Generic)

instance FromJSON ProductDetailRow
instance ToJSON ProductDetailRow

instance FromRow ProductDetailRow where
  fromRow = do
    _id           <- field
    _name         <- field
    _link         <- field
    _moreInfoJSON <- field :: RowParser T.Text
    _image        <- field
    _priceList    <- field
    _priceSpecial <- field
    _sourceJSON   <- field :: RowParser T.Text
    _date         <- field
    return $ ProductDetailRow
      _id
      (ProductDetail
        _name
        _link
        (fromMaybe [] (decoder _moreInfoJSON :: Maybe [T.Text]))
        _image
        (ProductPrice (Just _priceList) (Just _priceSpecial))
        (fromMaybe UnknownSource (decoder _sourceJSON :: Maybe Source))
        _date
      )
   where
    decoder :: FromJSON a => T.Text -> Maybe a
    decoder = decodeStrict . encodeUtf8

instance ToRow ProductDetailRow where
  toRow (ProductDetailRow id_ pd) = [SQLInteger $ fromIntegral id_] <> toRow pd

instance ToRow ProductDetail where
  toRow pd =
    [ SQLText $ name pd
    , SQLText $ link pd
    , SQLText $ encoder $ moreInfo pd
    , SQLText $ image pd
    , SQLInteger $ fromIntegral $ fromMaybe 0 (list $ price pd)
    , SQLInteger $ fromIntegral $ fromMaybe 0 (special $ price pd)
    , SQLText $ encoder $ source pd
    , SQLText $ date pd
    ]
   where
    encoder :: ToJSON a => a -> T.Text
    encoder = LT.toStrict . LBS.decodeUtf8 . encode
