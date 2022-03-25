{-|
  Description: An image button widget

An image button widget
  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}

{-# LANGUAGE TypeFamilies     #-}
module NixManager.View.ImageButton
  ( imageButton
  )
where

import           Data.Vector                            (Vector)
import qualified GI.Gtk                                 as Gtk
import           GI.Gtk.Declarative                     (Attribute,
                                                         CustomPatch (CustomKeep, CustomModify),
                                                         CustomWidget (CustomWidget),
                                                         Widget (Widget),
                                                         customAttributes,
                                                         customCreate,
                                                         customParams,
                                                         customPatch,
                                                         customSubscribe,
                                                         customWidget)
import           GI.Gtk.Declarative.Attributes.Internal (addSignalHandler)
import           NixManager.View.IconName               (IconName, nameToGtk)

-- | Create an image button widget
imageButton :: Vector (Attribute Gtk.Button e) -> IconName -> Widget e
imageButton customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.Button
  customCreate :: IconName -> IO (Gtk.Button, Gtk.Image)
  customCreate iconName = do
    w     <- Gtk.new Gtk.Button []
    -- Taken from https://hackage.haskell.org/package/gi-gtk-3.0.32/docs/src/GI.Gtk.Enums.html#IconSize
    image <- Gtk.imageNewFromIconName
      (Just (nameToGtk iconName))
      (fromIntegral (fromEnum Gtk.IconSizeButton))
    Gtk.buttonSetImage w (Just image)
    pure (w, image)
  customSubscribe _params _currentImage widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after currentImage
    | before == after = CustomKeep
    | otherwise = CustomModify $ \w -> do
      Gtk.widgetDestroy currentImage
      newImage <- Gtk.imageNewFromIconName
        (Just (nameToGtk after))
        (fromIntegral (fromEnum Gtk.IconSizeButton))
      Gtk.buttonSetImage w (Just newImage)
      pure newImage
