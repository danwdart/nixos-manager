{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-|
  Description: Contains the actual GUI (widgets) for the services tab
Contains the actual GUI (widgets) for the services tab
  -}
module NixManager.Services.View
  ( servicesBox
  , noticeBox
  )
where

import           Control.Lens                    ((^.))
import           Data.Default                    (def)
import           Data.Text                       (Text)
import qualified GI.Gtk                          as Gtk
import           GI.Gtk.Declarative              (Attribute ((:=)),
                                                  BoxChild (BoxChild), bin,
                                                  container,
                                                  defaultBoxChildProperties, on,
                                                  widget)
import           NixManager.ManagerEvent         (ManagerEvent (ManagerEventServices))
import           NixManager.Services.Event       (Event (EventDownloadCancel, EventDownloadStart, EventEditView, EventStateReload))
import           NixManager.Services.State       (State (StateDone, StateDownloading, StateInvalidExpr, StateInvalidOptions))
import           NixManager.View.GtkUtil         (expandAndFill)
import qualified NixManager.View.IconName        as IconName
import           NixManager.View.ImageButton     (imageButton)
import           NixManager.View.InformationBox  (informationBox)
import           NixManager.View.ProgressBar     (progressBar)
import           NixManager.View.ServiceEditView (editView)

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [BoxChild expandAndFill (servicesBox' (s ^. #serviceState) s)]

-- | What to display when the service definitions couldn't be parsed
invalidOptionsMessage :: Maybe Text -> Text
invalidOptionsMessage (Just e) =
  "Service definition file is invalid, possibly because of a corrupt download. You should try again. The error is:\n\n"
    <> e
invalidOptionsMessage Nothing =
  "Service definitions need to be downloaded first.\nPress the button below to start the download. It'll only take a few seconds, depending on your internet speed."

-- | The icon to display in case the service definitions aren't there or invalid
invalidOptionsIcon (Just _) = IconName.DialogError
invalidOptionsIcon Nothing  = IconName.EmblemDocuments

-- | The button text to display in case the service definitions aren't there or invalid
invalidOptionsButtonText (Just _) = "Retry Download"
invalidOptionsButtonText Nothing  = "Start Download"

-- | General function to display the notice box in case the service definitions aren't there or are invalid
noticeBox icon buttonEvent buttonIcon buttonText message = container
  Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #spacing := 10
  , #marginLeft := 40
  , #marginRight := 40
  , #marginTop := 5
  ]
  [ BoxChild def (informationBox False icon message)
  , BoxChild
    def
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ BoxChild def $ imageButton
          [ #label := buttonText
          , on #clicked buttonEvent
          , #alwaysShowImage := True
          ]
          buttonIcon
      ]
    )
  ]

-- | The services tab root
servicesBox' (StateDownloading ssdd) _ = container
  Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #spacing := 10
  , #marginLeft := 40
  , #marginRight := 40
  , #marginTop := 5
  ]
  [ BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := "Downloading services..."])
  , BoxChild defaultBoxChildProperties (progressBar [] (ssdd ^. #counter))
  , BoxChild
    defaultBoxChildProperties
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ widget
          Gtk.Button
          [ #label := "Cancel"
          , on #clicked (ManagerEventServices EventDownloadCancel)
          ]
      ]
    )
  ]
servicesBox' (StateInvalidExpr e) _ = bin Gtk.ScrolledWindow [] $ noticeBox
  IconName.DialogError
  (ManagerEventServices EventStateReload)
  IconName.EmblemDownloads
  "Reload service state"
  ("Your service expression file is not valid. Maybe you have edited it by hand and it's become corrupted?\nPlease fix the error and then press the button below. The error is:\n"
  <> e
  )
servicesBox' (StateInvalidOptions possibleError) _ =
  bin Gtk.ScrolledWindow [] $ noticeBox
    (invalidOptionsIcon possibleError)
    (ManagerEventServices EventDownloadStart)
    IconName.EmblemDownloads
    (invalidOptionsButtonText possibleError)
    (invalidOptionsMessage possibleError)
servicesBox' (StateDone sd) s =
  ManagerEventServices . EventEditView <$> editView sd s
