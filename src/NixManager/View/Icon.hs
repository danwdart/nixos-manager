{-|
  Description: An icon widget

An icon widget
  -}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module NixManager.View.Icon
  ( icon
  , IconProps(IconProps)
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

-- | An icon’s properties
data IconProps = IconProps {
    iconSize :: Gtk.IconSize -- ^ The icon’s desired size
  , iconName :: IconName -- ^ What icon to display
  } deriving(Eq)

-- | Create an icon widget
icon :: Vector (Attribute Gtk.Image e) -> IconProps -> Widget e
icon customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.Image
  customCreate :: IconProps -> IO (Gtk.Image, ())
  customCreate iconProps = do
    -- Taken from https://hackage.haskell.org/package/gi-gtk-3.0.32/docs/src/GI.Gtk.Enums.html#IconSize
    w <- Gtk.imageNewFromIconName
      (Just (nameToGtk (iconName iconProps)))
      (fromIntegral (fromEnum (iconSize iconProps)))
    pure (w, ())
  customSubscribe _params _currentImage widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after _internalState
    | before == after = CustomKeep
    | otherwise = CustomModify $ \w -> do
      Gtk.imageSetFromIconName w
                               (Just (nameToGtk (iconName after)))
                               (fromIntegral (fromEnum (iconSize after)))
      pure ()

