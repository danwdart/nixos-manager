{-|
  Description: Type for messages to be displayed in the GUI (errors, infos)
Type for messages to be displayed in the GUI (errors, infos)
  -}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Message
  ( MessageType
  , Message
  , messageWidget
  , errorMessage
  , infoMessage
  )
where

import           Control.Lens         (has, (^.))
import           Data.Generics.Labels ()
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import qualified GI.Gtk               as Gtk
import           GI.Gtk.Declarative   (Attribute ((:=)), Widget, classes,
                                       widget)

-- | Type of the message (determines the icon and/or background color)
data MessageType = ErrorMessage
                 | InfoMessage
                 deriving(Eq,Show, Generic)

-- | A message to be displayed in the GUI
data Message = Message {
    messageType :: MessageType
  , messageText :: Text
  }
  deriving(Eq,Show, Generic)

-- | Construct an error message
errorMessage :: Text -> Message
errorMessage = Message ErrorMessage

-- | Construct an info message
infoMessage :: Text -> Message
infoMessage = Message InfoMessage

-- | Create a nice-looking widget corresponding to the message given
messageWidget :: Message -> Widget e
messageWidget e = widget
  Gtk.Label
  [ #label := (e ^. #messageText)
  , #useMarkup := True
  , classes
    [ if has (#messageType . #_ErrorMessage) e
        then "error-message"
        else "info-message"
    ]
  ]

