{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMServices.State
  ( State(..)
  , initState
  )
where

import           Control.Lens                  (makePrisms)
import           Data.Text                     (Text)
import           Data.Validation               (Validation (Failure, Success))
import           GHC.Generics                  (Generic)
import           NixManager.HMServicesUtil     (locateOptionsFile,
                                                readPendingServicesFile)
import           NixManager.NixService         (makeServices)
import           NixManager.NixServiceOption   (readOptionsFile)
import           NixManager.Services.StateData (StateData (StateData))
import           NixManager.Util               (showText)

data State = NoHomeManager
           | InvalidHomeManager Text
           | HomeManagerPresent StateData
           deriving(Generic)

initState :: IO State
initState = locateOptionsFile >>= \case
  Nothing              -> pure NoHomeManager
  Just optionsFileName -> readOptionsFile optionsFileName >>= \case
    Failure e -> pure
      (InvalidHomeManager
        ("Your local options JSON file is corrupted. Please fix it, or delete it and run “home-manager switch” again. It’s stored in\n\n<tt>"
        <> showText optionsFileName
        <> "</tt>. The error is:\n\n<tt>"
        <> e
        <> "</tt>"
        )
      )
    Success options -> readPendingServicesFile >>= \case
      Failure e -> pure
        (InvalidHomeManager
          ("Your local service Nix configuration is corrupted. Please fix it. The error is: <tt>"
          <> e
          <> "</tt>"
          )
        )
      Success services -> pure
        (HomeManagerPresent
          (StateData (makeServices options) Nothing services mempty 0)
        )
