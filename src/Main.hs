{-# LANGUAGE OverloadedStrings, OverloadedLabels, ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}

-- Documentation :
-- https://hackage.haskell.org/package/gi-gtk

import Data.Text ()

import Control.Monad.IO.Class (MonadIO)
import Control.Monad

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base

import Data.Int (Int32)

printHello :: IO ()
printHello = putStrLn "Hello, World!"

getLines :: (Eq a, Num a) => a -> [IO Gtk.Box]
getLines 0 = []
getLines lineWidth = line : (getLines $ lineWidth - 1)
  where line = new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

listLines :: IO [Gtk.Box]
listLines = sequence $ getLines 2

appendTo :: Gtk.Box -> Gtk.Box -> IO ()
appendTo = Gtk.boxAppend

appendAllTo :: Gtk.Box -> [Gtk.Box] -> IO ()
appendAllTo layout ws = sequence_ $ map (appendTo layout) ws

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  layout <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  lines <- listLines
  appendAllTo layout lines

  letter1 <- new Gtk.Button [#label := "Hello World1!"]
  letter2 <- new Gtk.Button [#label := "Hello World2!"]
  -- Gtk.boxAppend firstLine letter
  Gtk.boxAppend (lines !! 0) letter1
  Gtk.boxAppend (lines !! 1) letter2
  on letter1 #clicked printHello
  on letter2 #clicked printHello

  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Virtual Keyboard"
                                 , #defaultHeight := 400
                                 , #defaultWidth := 1000
                                 , #child := layout
                                 ]
  #show w

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "virtual-keyboard.example"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  on app #activate $ activateApp app
  Gio.applicationRun app Nothing
  return ()
