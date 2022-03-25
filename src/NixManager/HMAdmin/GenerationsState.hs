{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains all state data for the home-manager generations view
Contains all state data for the home-manager generations view
  -}
module NixManager.HMAdmin.GenerationsState
  ( GenerationsState(..)
  , initGenerationsState
  )
where

import           Data.Text                          (Text)
import           Data.Validation                    (Validation (Failure, Success))
import           GHC.Generics                       (Generic)
import           NixManager.HMAdmin.GenerationsData (GenerationsData (GenerationsData))
import           NixManager.HMGenerations           (readGenerations)

-- | Current state of the generations view (depends on the success of the @home-manager generations@ call)
data GenerationsState = ValidGenerationsState GenerationsData
                      | InvalidGenerationsState Text
                      deriving(Generic)

-- | Initial state for the generations view (tries to read the generations, hence the side-effect)
initGenerationsState :: IO GenerationsState
initGenerationsState = readGenerations >>= \case
  Failure e ->
    pure $ InvalidGenerationsState ("Couldn't read generations: " <> e)
  Success g -> pure $ ValidGenerationsState (GenerationsData Nothing Nothing g)
