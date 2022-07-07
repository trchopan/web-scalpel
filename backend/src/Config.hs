{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Yaml
import           GHC.Generics                   ( Generic )
import           ProductDetail

data Config = Config
  { link   :: String
  , source :: ProductSource
  , name   :: String
  }
  deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

loadConfigs :: FilePath -> IO [Config]
loadConfigs fp =
  (decodeFileEither fp :: IO (Either ParseException [Config])) >>= \case
    Left  err -> error $ "Cannot decode Config: " ++ show err
    Right y   -> return y
