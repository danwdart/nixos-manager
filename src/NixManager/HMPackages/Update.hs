{-|
  Description: Contains the update logic for the Packages tab
Contains the update logic for the Packages tab
  -}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMPackages.Update
  ( updateEvent
  )
where

import           Control.Lens                    ((&), (.~), (?~), (^?))
import           Data.Validation                 (Validation (Failure, Success))
import           GI.Gtk.Declarative.App.Simple   (Transition (Transition))
import qualified NixManager.HMAdmin.Event        as HMAdminEvent
import           NixManager.HMPackages.Event     (Event (EventInstallCompleted, EventOperationCompleted, EventPackageEditView, EventReload, EventReloadFinished, EventUninstallCompleted))
import           NixManager.HMPackagesUtil       (installPackage,
                                                  readPackageCache,
                                                  uninstallPackage)
import           NixManager.ManagerEvent         (ManagerEvent (ManagerEventHMPackages),
                                                  hmAdminEvent, hmPackagesEvent,
                                                  liftUpdate, pureTransition)
import           NixManager.ManagerState         (ManagerState (..))
import           NixManager.Message              (Message, errorMessage,
                                                  infoMessage)
import qualified NixManager.View.PackageEditView as PEV
import           Prelude                         hiding (length, putStrLn)

-- | What message to display when the install operation completes
installCompletedMessage :: PEV.InstallationType -> Message
installCompletedMessage PEV.Uncancelled = infoMessage
  "Marked for installation! Head to the Admin tab to apply the changes."
installCompletedMessage PEV.Cancelled = infoMessage "Uninstall cancelled!"

-- | What message to display when the uninstall operation completes
uninstallCompletedMessage :: PEV.InstallationType -> Message
uninstallCompletedMessage PEV.Uncancelled = infoMessage
  "Marked for uninstall! Head to the Admin tab to apply the changes."
uninstallCompletedMessage PEV.Cancelled = infoMessage "Installation cancelled!"

-- | The actual update function
updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventOperationCompleted e completionType) =
  Transition (s & #hmPackagesState . #latestMessage ?~ e)
    $ case completionType of
        PEV.CompletionReload -> pure (hmAdminEvent HMAdminEvent.EventReload)
        PEV.CompletionPass   -> pure Nothing
updateEvent s (EventInstallCompleted cache installationType) = Transition
  (  s
  &  #hmPackagesState
  .  #packageCache
  .~ cache
  &  #hmPackagesState
  .  #selectedIdx
  .~ Nothing
  )
  (pure
    (hmPackagesEvent
      (EventOperationCompleted (installCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventUninstallCompleted cache installationType) = Transition
  (  s
  &  #hmPackagesState
  .  #packageCache
  .~ cache
  &  #hmPackagesState
  .  #selectedIdx
  .~ Nothing
  )
  (pure
    (hmPackagesEvent
      (EventOperationCompleted (uninstallCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventPackageEditView (PEV.EventInstall installationType)) =
  case s ^? #hmPackagesState . PEV.selectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition
      s
      do
        installResult <- installPackage selected
        cacheResult   <- readPackageCache
        case installResult *> cacheResult of
          Success newCache ->
            pure
              (hmPackagesEvent (EventInstallCompleted newCache installationType)
              )
          Failure e -> pure
            (hmPackagesEvent
              (EventOperationCompleted
                (errorMessage ("Install failed: " <> e))
                PEV.CompletionReload
              )
            )
updateEvent s (EventPackageEditView (PEV.EventUninstall installationType)) =
  case s ^? #hmPackagesState . PEV.selectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition
      s
      do
        uninstallResult <- uninstallPackage selected
        cacheResult     <- readPackageCache
        case uninstallResult *> cacheResult of
          Success newCache ->
            pure
              (hmPackagesEvent
                (EventUninstallCompleted newCache installationType)
              )
          Failure e -> pure
            (hmPackagesEvent
              (EventOperationCompleted
                (errorMessage ("Uninstall failed: " <> e))
                PEV.CompletionReload
              )
            )
updateEvent s EventReload = Transition
  s
  do
    cacheResult <- readPackageCache
    case cacheResult of
      Success newCache -> pure (hmPackagesEvent (EventReloadFinished newCache))
      Failure e        -> pure
        (hmPackagesEvent
          (EventOperationCompleted
            (errorMessage ("Couldn't reload packages cache: " <> e))
            PEV.CompletionPass
          )
        )
updateEvent s (EventReloadFinished newCache) =
  pureTransition (s & #hmPackagesState . #packageCache .~ newCache)
updateEvent s (EventPackageEditView e) = liftUpdate
  PEV.updateEvent
  #hmPackagesState
  (ManagerEventHMPackages . EventPackageEditView)
  s
  e
