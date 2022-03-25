{-|
  Description: Contains the actual GUI (widgets) for the Packages tab
Contains the actual GUI (widgets) for the Packages tab
  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.Packages.View
  ( packagesBox
  )
where

import           Control.Lens                    ((^.))
import           GI.Gtk.Declarative              (Widget)
import           NixManager.ManagerEvent         (ManagerEvent (ManagerEventPackages))
import           NixManager.ManagerState         (ManagerState)
import           NixManager.Packages.Event       (Event (EventPackageEditView))
import qualified NixManager.View.PackageEditView as PackageEditView

-- | The package list
packagesBox :: ManagerState -> Widget ManagerEvent
packagesBox s =
  ManagerEventPackages . EventPackageEditView <$> PackageEditView.packagesBox
    (s ^. #packagesState)
