{-# LANGUAGE DeriveGeneric #-}
{-|
  Description: Contains just the "BuildState" data type
Contains just the "BuildState" data type
  -}
module NixManager.Admin.BuildState
  ( BuildState(BuildState)
  )
where

import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)
import           NixManager.Password  (Password)
import           NixManager.Process   (ProcessData)

-- | Contains all the data corresponding to “some sudo program that’s currently running”
data BuildState = BuildState {
    counter     :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , processData :: ProcessData -- ^ The process data
  , password    :: Password -- ^ The password used to call the process. This is needed to cancel it again.
  } deriving(Generic)

