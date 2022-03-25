{-|
  Description: Contains all the state for the Packages tab
  -}
module NixManager.HMPackages.State
  ( State
  , initState
  , emptyState
  )
where

import           NixManager.HMPackagesUtil       (readPackageCache)
import           NixManager.Util                 (TextualError, ifSuccessIO)
import qualified NixManager.View.PackageEditView as PackageEditView

type State = PackageEditView.State

-- | The initial Packages tab state (needs to read the package cache, hence the side-effect)
initState :: IO (TextualError State)
initState =
  ifSuccessIO readPackageCache (pure . pure . PackageEditView.initState)

-- | An empty package state (we need this so we can "no-init" the home-manager package view if HM is disabled)
emptyState :: State
emptyState = PackageEditView.emptyState
