{-|
  Description: Contains all the state for the Services tab
Contains all the state for the Services tab
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.Services.State
  ( State(StateInvalidExpr, StateDownloading, StateDone, StateInvalidOptions)
  , initState
  , StateDownloadingData(StateDownloadingData)
  )
where

import           Data.Text                     (Text)
import           Data.Validation               (Validation (Failure, Success))
import           GHC.Generics                  (Generic)
import           NixManager.NixService         (makeServices)
import           NixManager.NixServiceOption   (locateOptionsFile,
                                                readOptionsFile)
import           NixManager.NixServicesUtil    (readLocalServiceFile)
import           NixManager.Services.Download  (DownloadState)
import           NixManager.Services.StateData (StateData (StateData))

-- | This contains the all data for the state “we’re currently downloading the services file”
data StateDownloadingData = StateDownloadingData {
    counter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , var     :: DownloadState -- ^ The actual download state
  } deriving(Generic)

data State = StateInvalidOptions (Maybe Text) -- ^ Parsing the service options file failed for some reason
           | StateInvalidExpr Text -- ^ Parsing the services Nix expression failed for some reason
           | StateDownloading StateDownloadingData -- ^ We’re currently downloading the options file
           | StateDone StateData -- ^ We have a valid options file
           deriving(Generic)

-- FIXME: Better happy path
-- | The initial Services tab state (needs to read the options file changes, hence the side-effect)
initState :: IO State
initState = do
  optionsFile' <- locateOptionsFile
  case optionsFile' of
    Nothing          -> pure (StateInvalidOptions Nothing)
    Just optionsFile -> do
      options' <- readOptionsFile optionsFile
      case options' of
        Failure e       -> pure (StateInvalidOptions (Just e))
        Success options -> do
          services' <- readLocalServiceFile
          case services' of
            Failure e -> pure (StateInvalidExpr e)
            Success services ->
              pure $ StateDone
                (StateData (makeServices options) Nothing services mempty 0)
