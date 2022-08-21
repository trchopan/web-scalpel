{-# LANGUAGE DeriveGeneric #-}

module ProductDetail where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

type ProductName = Text
type ProductLink = Text
type ProductImage = Text
type ProductDate = Text

data ProductPrice = ProductPrice
  { list    :: Maybe Integer
  , special :: Maybe Integer
  }
  deriving (Show, Generic)

instance ToJSON ProductPrice
instance FromJSON ProductPrice

newtype ProductMoreInfo = ProductMoreInfo [Text]
  deriving (Show, Generic)

instance FromJSON ProductMoreInfo
instance ToJSON ProductMoreInfo

data ProductDetail = ProductDetail
  { name     :: ProductName
  , link     :: ProductLink
  , image    :: ProductImage
  , moreInfo :: ProductMoreInfo
  , price    :: ProductPrice
  , source   :: ProductSource
  , date     :: ProductDate
  }
  deriving (Show, Generic)

instance ToJSON ProductDetail
instance FromJSON ProductDetail

data ProductSource = CellphonesVN | TheGioiDiDong | FptShop | UnknownSource
  deriving (Show, Generic)

instance ToJSON ProductSource where
  toJSON CellphonesVN  = "cellphones.com.vn"
  toJSON TheGioiDiDong = "thegioididong.com"
  toJSON FptShop       = "fptshop.com.vn"
  toJSON _             = "unknown-source"

instance FromJSON ProductSource where
  parseJSON "cellphones.com.vn" = return CellphonesVN
  parseJSON "thegioididong.com" = return TheGioiDiDong
  parseJSON "fptshop.com.vn"    = return FptShop
  parseJSON _                   = return UnknownSource

instance IsString ProductSource where
  fromString "cellphones.com.vn" = CellphonesVN
  fromString "thegioididong.com" = TheGioiDiDong
  fromString "fptshop.com.vn"    = FptShop
  fromString _                   = UnknownSource
