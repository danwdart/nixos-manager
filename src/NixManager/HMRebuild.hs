{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains functions relating to home-manager’s rebuild functionality
Contains functions relating to home-manager’s rebuild functionality
  -}
module NixManager.HMRebuild
  ( rebuild
  )
where

import           NixManager.Bash           (Expr (Command), (&&.))
import           NixManager.Constants      (appName)
import           NixManager.HMPackagesUtil (locatePendingPackagesFileMaybeCreate)
import           NixManager.HMRebuildMode  (HMRebuildMode (RebuildDrySwitch, RebuildSwitch))
import           NixManager.HMServicesUtil (locatePendingServicesFileMaybeCreate)
import           NixManager.PosixTools     (cp, mkdir)
import           NixManager.Process        (ProcessData, noStdin, runProcess)
import           System.Directory          (XdgDirectory (XdgCache),
                                            getXdgDirectory)

-- | The bash expression corresponding to a rebuild
rebuildExpr :: HMRebuildMode -> Expr
rebuildExpr RebuildSwitch    = Command "home-manager" ["switch"]
rebuildExpr RebuildDrySwitch = Command "home-manager" ["-n", "switch"]

-- | Given a rebuild mode, return the expression to rebuild (and save the result so we can compare changes)
rebuildAndSaveExpr :: HMRebuildMode -> IO Expr
rebuildAndSaveExpr RebuildSwitch = do
  cacheDir        <- getXdgDirectory XdgCache appName
  pendingPackages <- locatePendingPackagesFileMaybeCreate
  pendingServices <- locatePendingServicesFileMaybeCreate
  pure
    (   mkdir True [cacheDir]
    &&. rebuildExpr RebuildSwitch
    &&. cp pendingPackages cacheDir
    &&. cp pendingServices cacheDir
    )
rebuildAndSaveExpr RebuildDrySwitch = pure (rebuildExpr RebuildDrySwitch)

-- | Given a rebuild mode, rebuild (and save the result so we can compare changes)
rebuild :: HMRebuildMode -> IO ProcessData
rebuild mode = rebuildAndSaveExpr mode >>= runProcess noStdin
