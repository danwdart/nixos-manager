{-|
  Description: Contains the actual GUI (widgets) for the Packages tab
Contains the actual GUI (widgets) for the Packages tab
  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.HMPackages.View
  ( packagesBox
  )
where

import           Control.Lens                    ((^.))
import           GI.Gtk.Declarative              (Widget)
import           NixManager.HMPackages.Event     (Event (EventPackageEditView))
import           NixManager.ManagerEvent         (ManagerEvent (ManagerEventHMPackages))
import           NixManager.ManagerState         (ManagerState)
import qualified NixManager.View.PackageEditView as PackageEditView

-- | The package list
packagesBox :: ManagerState -> Widget ManagerEvent
packagesBox s =
  ManagerEventHMPackages . EventPackageEditView <$> PackageEditView.packagesBox
    (s ^. #hmPackagesState)
