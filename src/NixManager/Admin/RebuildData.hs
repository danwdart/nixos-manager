{-|
  Description: Contains all data for the rebuild GUI
Contains all data for the rebuild GUI
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.Admin.RebuildData
  ( RebuildData(..)
  , initialRebuildData
  )
where

import           Data.Generics.Labels         ()
import           GHC.Generics                 (Generic)
import           NixManager.Admin.BuildState  (BuildState)
import           NixManager.Process           (ProcessOutput)
import           NixManager.View.DetailsState (DetailsState (DetailsContracted))

-- | Contains all data for the rebuild GUI
data RebuildData = RebuildData {
    processOutput        :: ProcessOutput  -- ^ Output of the current or last rebuild process (possibly empty)
  , buildState           :: Maybe BuildState  -- ^ Contains the current build state of the rebuild
  , activeRebuildModeIdx :: Int -- ^ Index of the active rebuild mode, see "NixManager.NixRebuildMode"
  , detailsState         :: DetailsState -- ^ Are the Details expanded?
  , doUpdate             :: Bool -- ^ Shall we do an update?
  , doRollback           :: Bool -- ^ Shall we do a rollback
  } deriving(Generic)

-- | The initial rebuild state
initialRebuildData :: RebuildData
initialRebuildData = RebuildData mempty Nothing 0 DetailsContracted False False
