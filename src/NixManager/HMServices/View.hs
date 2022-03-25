{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMServices.View
  ( servicesBox
  )
where

import           Control.Lens                    ((^.))
import qualified GI.Gtk                          as Gtk
import           GI.Gtk.Declarative              (BoxChild (BoxChild), bin,
                                                  container)
import           NixManager.HMServices.Event     (Event (EventEditView, EventReload))
import           NixManager.HMServices.State     (State (HomeManagerPresent, InvalidHomeManager, NoHomeManager))
import           NixManager.ManagerEvent         (ManagerEvent (ManagerEventHMServices))
import           NixManager.Services.View        (noticeBox)
import           NixManager.View.GtkUtil         (expandAndFill)
import qualified NixManager.View.IconName        as IconName
import           NixManager.View.ServiceEditView (editView)

servicesBox' NoHomeManager _ = bin Gtk.ScrolledWindow [] $ noticeBox
  IconName.DialogError
  (ManagerEventHMServices EventReload)
  IconName.ViewRefresh
  "Reload"
  "You don't have home-manager installed, or it isn’t configured properly. INSERT MORE DOCS HERE."
servicesBox' (InvalidHomeManager errorMessage) _ =
  bin Gtk.ScrolledWindow [] $ noticeBox IconName.DialogError
                                        (ManagerEventHMServices EventReload)
                                        IconName.ViewRefresh
                                        "Reload"
                                        errorMessage
servicesBox' (HomeManagerPresent sd) s =
  ManagerEventHMServices . EventEditView <$> editView sd s

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [BoxChild expandAndFill (servicesBox' (s ^. #hmServiceState) s)]
