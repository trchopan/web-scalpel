{-# LANGUAGE LambdaCase #-}
module Scrapers where

import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( (<&>) )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text
                                                , replace
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           ProductDetail                  ( ProductDetail(ProductDetail)
                                                , ProductImage
                                                , ProductLink
                                                , ProductMoreInfo(..)
                                                , ProductName
                                                , ProductPrice(ProductPrice)
                                                , ProductSource
                                                  ( CellphonesVN
                                                  , FptShop
                                                  , TheGioiDiDong
                                                  )
                                                )
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
scrapeWithTimeAndSource inputStr entry source date =
  return
    $   (scrapeStringLike inputStr entry :: (Maybe [ScraperResult]))
    >>= (Just . map resultToProductDetail)
 where
  resultToProductDetail :: ScraperResult -> ProductDetail
  resultToProductDetail (ScraperResult name link image moreInfo price) =
    ProductDetail name link image moreInfo price source date


cellphoneScraper :: String -> Text -> IO (Maybe [ProductDetail])
cellphoneScraper inputStr = scrapeWithTimeAndSource
  inputStr
  (cellphoneSScraperV1 <|> cellphoneSScrapperV2)
  CellphonesVN
 where
  priceToInt :: String -> Maybe Integer
  priceToInt =
    readMaybe
      . unpack
      . replace "\160\8363" ""
      . replace "."         ""
      . (strip . fromString)

  cellphoneSScraperV1 :: Scraper String [ScraperResult]
  cellphoneSScraperV1 =
    chroot ("div" @: [hasClass "san-pham-cate"])
      $   chroots ("div" @: [hasClass "item-product"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> (getMoreInfo <|> pure (ProductMoreInfo []))
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

  cellphoneSScrapperV2 :: Scraper String [ScraperResult]
  cellphoneSScrapperV2 =
    chroot ("div" @: [hasClass "product-list-filter"])
      $   chroots ("div" @: [hasClass "product-info-container"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> pure (ProductMoreInfo [])
      <*> (getPrice <|> getPriceWithSpecial)
   where
    getName :: Scraper String Text
    getName =
      chroot ("div" @: [hasClass "product__name"])
        $   text anySelector
        <&> (strip . fromString)

    getLink :: Scraper String Text
    getLink = chroot "a" $ attr "href" anySelector <&> fromString

    getImage :: Scraper String Text
    getImage =
      chroot ("div" @: [hasClass "product__image"])
        $   chroot "img"
        $   attr "data-src" anySelector
        <&> fromString

    getPrice :: Scraper String ProductPrice
    getPrice = chroot ("div" @: [hasClass "box-info__box-price"]) $ do
      price <- text ("p" @: [hasClass "product__price"])
        <|> text ("p" @: [hasClass "product__price--show"])
      return $ ProductPrice (priceToInt price) (priceToInt price)

    getPriceWithSpecial :: Scraper String ProductPrice
    getPriceWithSpecial =
      chroot ("div" @: [hasClass "box-info__box-price"]) $ do
        special <- text $ "p" @: [hasClass "product__price--show"]
        price   <- text $ "p" @: [hasClass "product__price--through"]
        return $ ProductPrice (priceToInt price) (priceToInt special)


tgddScraper :: String -> Text -> IO (Maybe [ProductDetail])
tgddScraper inputStr = scrapeWithTimeAndSource inputStr
                                               scrapeEntry
                                               TheGioiDiDong
 where
  priceToInt :: String -> Maybe Integer
  priceToInt =
    readMaybe
      . unpack
      . replace "\8363" ""
      . replace "."     ""
      . (strip . fromString)

  scrapeEntry :: Scraper String [ScraperResult]
  scrapeEntry =
    chroot ("ul" @: [hasClass "listproduct"])
      $   chroots ("li" @: [hasClass "item"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> (getMoreInfo <|> pure (ProductMoreInfo []))
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

    getMoreInfo :: Scraper String ProductMoreInfo
    getMoreInfo =
      chroot ("div" @: [hasClass "prods-group"])
        $   chroot "ul"
        $   chroot ("li" @: [hasClass "item", hasClass "act"])
        $   text anySelector
        <&> (map strip . splitOn "/" . fromString)
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


fptScraper :: String -> Text -> IO (Maybe [ProductDetail])
fptScraper inputStr = scrapeWithTimeAndSource inputStr
                                              (fptScraperV1 <|> fptScraperV2)
                                              FptShop
 where
  priceToInt :: String -> Maybe Integer
  priceToInt =
    readMaybe . unpack . replace " ₫" "" . replace "." "" . (strip . fromString)

  fptScraperV1 :: Scraper String [ScraperResult]
  fptScraperV1 =
    chroot ("div" @: [hasClass "cdt-product-wrapper"])
      $   chroots ("div" @: [hasClass "cdt-product"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> (getMoreInfo <|> pure (ProductMoreInfo []))
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

  fptScraperV2 :: Scraper String [ScraperResult]
  fptScraperV2 =
    chroot ("div" @: [hasClass "product-grid"])
      $   chroots ("div" @: [hasClass "product"])
      $   ScraperResult
      <$> getName
      <*> getLink
      <*> getImage
      <*> (getMoreInfo <|> pure (ProductMoreInfo []))
      <*> (getPrice <|> getPricePromo)
   where
    getName :: Scraper String Text
    getName =
      chroot ("div" @: [hasClass "product_info"])
        $   text "h3"
        <&> (strip . fromString)

    getLink :: Scraper String Text
    getLink =
      chroot ("div" @: [hasClass "product_info"])
        $   chroot "a"
        $   attr "href" anySelector
        <&> (++) "https://fptshop.com.vn"
        <&> fromString

    getMoreInfo :: Scraper String ProductMoreInfo
    getMoreInfo =
      chroot ("div" @: [hasClass "product__config__param"])
        $   texts "spans"
        <&> map (strip . fromString)
        <&> ProductMoreInfo


    getImage :: Scraper String Text
    getImage =
      chroot ("div" @: [hasClass "product_img"])
        $   chroot "img"
        $   attr "src" anySelector
        <&> fromString

    getPrice :: Scraper String ProductPrice
    getPrice = chroot ("div" @: [hasClass "product__price"]) $ do
      price <- chroot ("div" @: [hasClass "price"]) $ text anySelector
      return $ ProductPrice (priceToInt price) (priceToInt price)

    getPricePromo :: Scraper String ProductPrice
    getPricePromo = chroot ("div" @: [hasClass "product_timing"]) $ do
      special <- chroot ("div" @: [hasClass "product_progress"])
        $ text anySelector
      price <- chroot ("div" @: [hasClass "product_strike"]) $ text "strike"
      return $ ProductPrice (priceToInt price) (priceToInt special)
