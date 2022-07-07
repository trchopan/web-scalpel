{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ProductDetail where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text
                                                , replace
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           GHC.Generics                   ( Generic )
import           Text.HTML.Scalpel              ( (@:)
                                                , Scraper
                                                , anySelector
                                                , attr
                                                , chroot
                                                , chroots
                                                , hasClass
                                                , scrapeStringLike
                                                , text
                                                , texts
                                                )
import           Text.Read                      ( readMaybe )

type ProductName = Text
type ProductLink = Text
type ProductImage = Text
type ProductDate = Text

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

data ProductPrice = ProductPrice
  { list    :: Maybe Integer
  , special :: Maybe Integer
  }
  deriving (Show, Generic)

instance ToJSON ProductPrice
instance FromJSON ProductPrice

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

scraperForSource
  :: ProductSource -> Maybe (String -> Text -> IO (Maybe [ProductDetail]))
scraperForSource = \case
  CellphonesVN  -> Just cellphoneScraper
  TheGioiDiDong -> Just tgddScraper
  FptShop       -> Just fptScraper
  _             -> Nothing

data ScraperResult = ScraperResult ProductName
                                   ProductLink
                                   ProductImage
                                   ProductMoreInfo
                                   ProductPrice

scrapeWithTimeAndSource
  :: String
  -> Scraper String [ScraperResult]
  -> ProductSource
  -> Text
  -> IO (Maybe [ProductDetail])
scrapeWithTimeAndSource str entry source date = do
  let maybeResult = scrapeStringLike str entry
  return $ case maybeResult of
    Nothing     -> Nothing
    Just result -> Just $ map
      (\(ScraperResult name link image moreInfo price) ->
        ProductDetail name link image moreInfo price source date
      )
      result

cellphoneScraper :: String -> Text -> IO (Maybe [ProductDetail])
cellphoneScraper str = scrapeWithTimeAndSource str scrapeEntry CellphonesVN
 where
  scrapeEntry :: Scraper String [ScraperResult]
  scrapeEntry =
    chroots ("div" @: [hasClass "item-product"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> getMoreInfo
      <*> getPrice
   where
    getName :: Scraper String Text
    getName =
      chroot ("div" @: [hasClass "item-product__box-name"])
        $   text anySelector
        <&> (strip . fromString)

    getLink :: Scraper String Text
    getLink =
      chroot ("div" @: [hasClass "item-product__box-name"])
        $   chroot "a"
        $   attr "href" anySelector
        <&> fromString

    getMoreInfo :: Scraper String ProductMoreInfo
    getMoreInfo =
      chroot ("div" @: [hasClass "item-product__more-info"]) --
        $   texts "p"
        <&> map (strip . fromString)
        <&> ProductMoreInfo


    getImage :: Scraper String Text
    getImage =
      chroot ("div" @: [hasClass "item-product__box-img"])
        $   chroot "img"
        $   attr "data-src" anySelector
        <&> fromString

    getPrice :: Scraper String ProductPrice
    getPrice = chroot ("div" @: [hasClass "item-product__box-price"]) $ do
      special <- text $ "p" @: [hasClass "special-price"]
      price   <- text $ "p" @: [hasClass "old-price"]
      return $ ProductPrice (priceToInt price) (priceToInt special)
     where
      priceToInt :: String -> Maybe Integer
      priceToInt =
        readMaybe
          . unpack
          . replace "\160\8363" ""
          . replace "."         ""
          . (strip . fromString)

tgddScraper :: String -> Text -> IO (Maybe [ProductDetail])
tgddScraper str = scrapeWithTimeAndSource str scrapeEntry TheGioiDiDong
 where
  scrapeEntry :: Scraper String [ScraperResult]
  scrapeEntry =
    chroot ("ul" @: [hasClass "listproduct"])
      $   chroots ("li" @: [hasClass "item"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> (getMoreInfoProd <|> getMoreInfoItem)
      <*> getPrice
   where
    getName :: Scraper String Text
    getName = text "h3" <&> (strip . fromString)

    getLink :: Scraper String Text
    getLink =
      chroot "a"
        $   attr "href" anySelector
        <&> (++) "https://www.thegioididong.com"
        <&> fromString

    getMoreInfoProd :: Scraper String ProductMoreInfo
    getMoreInfoProd =
      chroot ("div" @: [hasClass "prods-group"])
        $   chroot "ul"
        $   chroot ("li" @: [hasClass "item", hasClass "act"])
        $   text anySelector
        <&> (map strip . splitOn "/" . fromString)
        <&> ProductMoreInfo

    getMoreInfoItem :: Scraper String ProductMoreInfo
    getMoreInfoItem =
      chroot ("div" @: [hasClass "item-compare"]) --
        $   texts "span"
        <&> map (strip . fromString)
        <&> ProductMoreInfo


    getImage :: Scraper String Text
    getImage =
      chroot ("div" @: [hasClass "item-img"])
        $   chroot "img"
        $   attr "src" anySelector
        <&> fromString

    getPrice :: Scraper String ProductPrice
    getPrice = do
      special <- chroot ("strong" @: [hasClass "price"]) $ text anySelector
      price   <- chroot ("div" @: [hasClass "box-p"])
        $ text ("p" @: [hasClass "price-old"])
      let specialOrPrice = if null special then price else special
      return $ ProductPrice (priceToInt price) (priceToInt specialOrPrice)

    priceToInt :: String -> Maybe Integer
    priceToInt =
      readMaybe
        . unpack
        . replace "\8363" ""
        . replace "."     ""
        . (strip . fromString)


fptScraper :: String -> Text -> IO (Maybe [ProductDetail])
fptScraper str = scrapeWithTimeAndSource str scrapeEntry FptShop
 where
  scrapeEntry :: Scraper String [ScraperResult]
  scrapeEntry =
    chroots ("div" @: [hasClass "cdt-product"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> getMoreInfo
      <*> (getPrice <|> getPricePromo)
   where
    getName :: Scraper String Text
    getName =
      chroot ("div" @: [hasClass "cdt-product__info"])
        $   text "h3"
        <&> (strip . fromString)

    getLink :: Scraper String Text
    getLink =
      chroot ("div" @: [hasClass "cdt-product__info"])
        $   chroot "a"
        $   attr "href" anySelector
        <&> (++) "https://fptshop.com.vn"
        <&> fromString

    getMoreInfo :: Scraper String ProductMoreInfo
    getMoreInfo =
      chroot ("div" @: [hasClass "cdt-product__config__param"])
        $   texts "spans"
        <&> map (strip . fromString)
        <&> ProductMoreInfo


    getImage :: Scraper String Text
    getImage =
      chroot ("div" @: [hasClass "cdt-product__img"])
        $   chroot "img"
        $   attr "src" anySelector
        <&> fromString

    getPrice :: Scraper String ProductPrice
    getPrice = chroot ("div" @: [hasClass "cdt-product__price"]) $ do
      price <- chroot ("div" @: [hasClass "price"]) $ text anySelector
      return $ ProductPrice (priceToInt price) (priceToInt price)

    getPricePromo :: Scraper String ProductPrice
    getPricePromo = chroot ("div" @: [hasClass "cdt-product__show-promo"]) $ do
      special <- chroot ("div" @: [hasClass "progress"]) $ text anySelector
      price   <- chroot ("div" @: [hasClass "strike-price"]) $ text "strike"
      return $ ProductPrice (priceToInt price) (priceToInt special)

    priceToInt :: String -> Maybe Integer
    priceToInt =
      readMaybe
        . unpack
        . replace " ₫" ""
        . replace "."  ""
        . (strip . fromString)
