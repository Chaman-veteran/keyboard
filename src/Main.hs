{-# LANGUAGE OverloadedStrings, OverloadedLabels, ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}

-- Documentation :
-- https://hackage.haskell.org/package/gi-gtk


import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, singleton)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base (new, AttrOp((:=)), after, on)

type CharLayout = [[Text]]
type ButtonLayout = [[Gtk.Button]]

qwertyLayout :: CharLayout
qwertyLayout = map (map singleton)
    [['q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'],
     ['a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';'],
     ['z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.']
    ]

printLabel :: Gtk.Button -> IO ()
printLabel b = do
    label <- Gtk.buttonGetLabel b
    print $ fromMaybe "Missing label" label

listLines :: Int -> IO [Gtk.Box]
listLines n = sequence $ replicate n $ new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

listButtons :: Int -> IO [Gtk.Button]
listButtons n = sequence $ replicate n $ new Gtk.Button []

initButton :: Gtk.Button -> Text -> IO ()
initButton button label = do
    after button #clicked $ printLabel button
    Gtk.buttonSetLabel button label

labelsToButtons :: [Text] -> [Gtk.Button] -> IO ()
labelsToButtons labels buttons = sequence_ $ zipWith initButton buttons labels

appendTo :: Gtk.IsWidget w => Gtk.Box -> w -> IO ()
appendTo = Gtk.boxAppend

appendAllTo :: Gtk.IsWidget w => Gtk.Box -> [w] -> IO ()
appendAllTo keyboard ws = sequence_ $ map (appendTo keyboard) ws

applyLayout :: CharLayout -> IO ButtonLayout
applyLayout = sequence . map (\l -> listButtons $ length l) 

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  keyboard <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  lines <- listLines (length qwertyLayout)
  letters' <- applyLayout qwertyLayout
  sequence_ $ map (uncurry labelsToButtons) $ zip qwertyLayout letters'

  appendAllTo keyboard lines
  sequence_ $ map (uncurry appendAllTo) $ zip lines letters'

  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Virtual Keyboard"
                                 , #defaultHeight := 400
                                 , #defaultWidth := 1000
                                 , #child := keyboard
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
