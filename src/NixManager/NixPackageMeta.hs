{-|
  Description: Metadata for a Nix package. This is a companion module to "NixManager.NixPackage"
Metadata for a Nix package. This is a companion module to "NixManager.NixPackage"
  -}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixPackageMeta
  ( NixPackageMeta(NixPackageMeta)
  )
where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON, Value (Object), parseJSON, (.:))
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

-- | Metadata for a Nix package. This is a companion module to "NixManager.NixPackage"
data NixPackageMeta = NixPackageMeta {
    name        :: Text -- ^ Package name
  , version     :: Text -- ^ Package version
  , description :: Text -- ^ Package description
  } deriving(Eq,Show, Generic)

instance FromJSON NixPackageMeta where
  parseJSON (Object v) =
    NixPackageMeta <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

