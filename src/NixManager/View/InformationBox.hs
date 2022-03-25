{-|
  Description: Functions to display informational boxes

Functions to display informational boxes
  -}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module NixManager.View.InformationBox
  ( informationBox
  )
where

import           Data.Default                 (def)
import           Data.Text                    (Text)
import qualified GI.Gtk                       as Gtk
import           GI.Gtk.Declarative           (Attribute ((:=)),
                                               BoxChild (BoxChild), Container,
                                               FromWidget, classes, container,
                                               widget)
import           GI.Gtk.Declarative.Container (Children)
import           NixManager.View.Icon         (IconProps (IconProps), icon)
import qualified NixManager.View.IconName     as IconName

-- | Display a box with an icon and a descriptive text next to it, possibly using Pango markup
informationBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => Bool -- ^ Whether to use pango markup
  -> IconName.IconName -- ^ Icon to display
  -> Text -- ^ Message to display
  -> target event
informationBox useMarkup iconName message = container
  Gtk.Box
  [ #orientation := Gtk.OrientationHorizontal
  , #spacing := 15
  , #halign := Gtk.AlignCenter
  ]
  [ BoxChild def $ icon [] (IconProps Gtk.IconSizeDialog iconName)
  , BoxChild def $ widget
    Gtk.Label
    [ #label := message
    , #wrap := True
    , #useMarkup := useMarkup
    , #halign := Gtk.AlignCenter
    , classes ["nixos-manager-italic"]
    ]
  ]
