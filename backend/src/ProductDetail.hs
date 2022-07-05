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

data ProductDetail = ProductDetail
  { name     :: Text
  , link     :: Text
  , moreInfo :: [Text]
  , image    :: Text
  , price    :: ProductPrice
  , source   :: Source
  , date     :: Text
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

data Source = CellphonesVN | TheGioiDiDong | FptShop | UnknownSource
  deriving (Show, Generic)

instance IsString Source where
  fromString "cellphones.com.vn" = CellphonesVN
  fromString "thegioididong.com" = TheGioiDiDong
  fromString "fptshop.com.vn"    = FptShop
  fromString _                   = UnknownSource


instance ToJSON Source where
  toJSON CellphonesVN  = "cellphones.com.vn"
  toJSON TheGioiDiDong = "thegioididong.com"
  toJSON FptShop       = "fptshop.com.vn"
  toJSON _             = "unknown-source"

instance FromJSON Source where
  parseJSON "cellphones.com.vn" = return CellphonesVN
  parseJSON "thegioididong.com" = return TheGioiDiDong
  parseJSON "fptshop.com.vn"    = return FptShop
  parseJSON _                   = return UnknownSource

scraperForSource :: Source -> Maybe (String -> Text -> IO (Maybe [ProductDetail]))
scraperForSource = \case
  CellphonesVN  -> Just cellphoneScraper
  TheGioiDiDong -> Just tgddScraper
  FptShop       -> Just fptScraper
  _             -> Nothing

data ScraperResult = ScraperResult Text Text [Text] Text ProductPrice

scrapeWithTimeAndSource
  :: String
  -> Scraper String [ScraperResult]
  -> Source
  -> Text
  -> IO (Maybe [ProductDetail])
scrapeWithTimeAndSource str entry source date = do
  let maybeResult = scrapeStringLike str entry
  return $ case maybeResult of
    Nothing     -> Nothing
    Just result -> Just $ map
      (\(ScraperResult name link moreInfo image price) ->
        ProductDetail name link moreInfo image price source date
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
      <*> getMoreInfo
      <*> getImage
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

    getMoreInfo :: Scraper String [Text]
    getMoreInfo =
      chroot ("div" @: [hasClass "item-product__more-info"]) --
        $   texts "p"
        <&> map (strip . fromString)


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
      <*> (getMoreInfoProd <|> getMoreInfoItem)
      <*> getImage
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

    getMoreInfoProd :: Scraper String [Text]
    getMoreInfoProd =
      chroot ("div" @: [hasClass "prods-group"])
        $   chroot "ul"
        $   chroot ("li" @: [hasClass "item", hasClass "act"])
        $   text anySelector
        <&> (map strip . splitOn "/" . fromString)

    getMoreInfoItem :: Scraper String [Text]
    getMoreInfoItem =
      chroot ("div" @: [hasClass "item-compare"]) --
        $   texts "span"
        <&> map (strip . fromString)


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
      <*> getMoreInfo
      <*> getImage
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

    getMoreInfo :: Scraper String [Text]
    getMoreInfo =
      chroot ("div" @: [hasClass "cdt-product__config__param"])
        $   texts "spans"
        <&> map (strip . fromString)


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
