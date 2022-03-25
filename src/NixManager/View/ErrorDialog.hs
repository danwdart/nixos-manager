{-|
  Description: An error dialog which is displayed in case initialization fails

An error dialog which is displayed in case initialization fails
 -}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.ErrorDialog
  ( runErrorDialog
  )
where

import           Control.Monad                 (void)
import           Data.Text                     (Text)
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative            (Attribute ((:=)),
                                                BoxChild (BoxChild), bin,
                                                classes, container, expand,
                                                fill, on, widget)
import           GI.Gtk.Declarative.App.Simple (App (App), AppView,
                                                Transition (Exit), initialState,
                                                inputs, run, update, view)
import           NixManager.View.GtkUtil       (expandAndFill, paddedAround)
import           Text.Wrap                     (WrapSettings (WrapSettings),
                                                wrapText)

-- | There’s just one event, to exit the dialog
data Event = ExitEvent

-- | The error dialog’s view function
errorDialog :: Text -> () -> AppView Gtk.Dialog Event
errorDialog e _ =
  let msgLabel = widget
        Gtk.Label
        [ #label := wrapText (WrapSettings True False) 80 e
        , classes ["startup-error-message"]
        ]
  in  bin
          Gtk.Dialog
          [ #title := "An error occurred"
          , on #deleteEvent (const (True, ExitEvent))
          , #widthRequest := 300
          , #heightRequest := 200
          ]
        $ container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            [ BoxChild expandAndFill (paddedAround 20 msgLabel)
            , paddedAround 5 $ container
              Gtk.Box
              [#halign := Gtk.AlignEnd, #spacing := 5]
              [ widget
                  Gtk.Button
                  [ #label := "Okay, let me fix this real quick"
                  , on #clicked ExitEvent
                  ]
              ]
            ]

-- | Display an error dialog, wait for the user to close it again.
runErrorDialog :: Text -> IO ()
runErrorDialog e = void $ run App { view         = errorDialog e
                                  , update       = \_ _ -> Exit
                                  , inputs       = []
                                  , initialState = ()
                                  }
