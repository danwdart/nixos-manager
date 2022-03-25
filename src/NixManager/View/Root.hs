{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: The root of the view hierarchy

The root of the view hierarchy
  -}
module NixManager.View.Root
  ( view'
  )
where

import           Control.Lens                  ((^.))
import           Data.Default                  (def)
import           Data.Vector                   (Vector)
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative            (Attribute ((:=)),
                                                BoxChild (BoxChild), bin,
                                                container, notebook, on,
                                                pageWithTab, widget)
import           GI.Gtk.Declarative.App.Simple (AppView)
import qualified NixManager.Admin.View         as AdminView
import qualified NixManager.HMAdmin.View       as HMAdminView
import qualified NixManager.HMPackages.View    as HMPackagesView
import qualified NixManager.HMServices.View    as HMServicesView
import           NixManager.ManagerEvent       (ManagerEvent (ManagerEventClosed))
import           NixManager.ManagerState       (ManagerState)
import qualified NixManager.Packages.View      as PackagesView
import           NixManager.ProgramArguments   (ProgramArguments)
import qualified NixManager.Services.View      as ServicesView
import           NixManager.View.Icon          (IconProps (IconProps), icon)
import qualified NixManager.View.IconName      as IconName

-- | The main window’s attributes
windowAttributes :: Vector (Attribute Gtk.Window ManagerEvent)
windowAttributes =
  [ #title := "nixos-manager 1.0"
  , on #deleteEvent (const (True, ManagerEventClosed))
  , #widthRequest := 1024
  , #heightRequest := 768
  ]

-- | A label with an image next to it (used in the notebook’s tab headers)
imagedLabel iconProps text = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  [ BoxChild def (icon [] iconProps)
  , BoxChild def (widget Gtk.Label [#label := text, #valign := Gtk.AlignCenter])
  ]

-- | The root view function
view' :: ProgramArguments -> ManagerState -> AppView Gtk.Window ManagerEvent
view' pa s =
  let adminTab = pageWithTab
        (imagedLabel
          (IconProps Gtk.IconSizeButton IconName.ApplicationsSystem)
          "Administration"
        )
        (AdminView.adminBox s)
      packagesTab = pageWithTab
        (imagedLabel (IconProps Gtk.IconSizeButton IconName.PackageXGeneric)
                     "Add/Remove Software"
        )
        (PackagesView.packagesBox s)
      servicesTab = pageWithTab
        (imagedLabel (IconProps Gtk.IconSizeButton IconName.PreferencesOther)
                     "Configure your system"
        )
        (ServicesView.servicesBox s)
      hmAdminTab = pageWithTab
        (imagedLabel
          (IconProps Gtk.IconSizeButton IconName.ApplicationsSystem)
          "Home Administration"
        )
        (HMAdminView.adminBox s)
      hmPackagesTab = pageWithTab
        (imagedLabel (IconProps Gtk.IconSizeButton IconName.PackageXGeneric)
                     "Add/Remove Software"
        )
        (HMPackagesView.packagesBox s)
      hmServicesTab = pageWithTab
        (imagedLabel (IconProps Gtk.IconSizeButton IconName.UserHome)
                     "Configure your Home"
        )
        (HMServicesView.servicesBox s)
      windowContents = if pa ^. #useHomeManager
        then notebook [] [hmAdminTab, hmPackagesTab, hmServicesTab]
        else notebook [] [adminTab, packagesTab, servicesTab]
  in  bin Gtk.Window windowAttributes windowContents
