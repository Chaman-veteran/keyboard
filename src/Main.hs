{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  Main
--
--
--  Virtual keyboard for linux.
--
-----------------------------------------------------------------------------

-- Documentation:
-- https://hackage.haskell.org/package/gi-gtk


import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, singleton)
import Data.Maybe (fromMaybe, fromJust)
import Data.Int (Int32)
import Data.Aeson (Array, Value(Object, String, Array), decode)
import Data.Vector (Vector)
import Data.Aeson.KeyMap (Key)
import qualified Data.Text as T (head)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM ((!?))
import qualified Data.ByteString.Lazy as B (readFile)

import GI.Gtk (Button, Box, IsWidget, Application)
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base (new, AttrOp((:=)), after, on)

import System.IO (hPutChar, openTempFile, BufferMode(NoBuffering))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import GHC.IO.Handle (Handle, hSetBuffering)

type TextLayout = V.Vector (V.Vector Text)
type ButtonLayout = V.Vector (V.Vector Gtk.Button)

tempFile :: IO (Handle)
tempFile = do
  tempDir <- getCanonicalTemporaryDirectory
  hdl <- snd <$> openTempFile tempDir "keyboard"
  hSetBuffering hdl NoBuffering
  return hdl

printLabel :: Handle -> Button -> IO ()
printLabel hdl b = do
    label <- Gtk.buttonGetLabel b
    case label of
      Nothing -> return ()
      Just c -> hPutChar hdl $ T.head c
    print $ fromMaybe "Missing label" label

-- | Extracts the text value out of a json
getFromJSON :: Key -> Value -> Text
getFromJSON key (Object o) = (\(String s) -> s) . fromJust $ o KM.!? key

qwertyLayout :: IO TextLayout
qwertyLayout = do
  layoutB <- B.readFile "layouts/qwerty.json"
  let layoutArray = fromJust (decode layoutB :: Maybe Array)
  let fromJSON = V.map (\(Array v) -> V.map (getFromJSON "label") v) layoutArray
  return fromJSON

listLines :: Int -> IO (Vector Box)
listLines n = sequence $ V.replicate n $ new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

listButtons :: Int -> IO (Vector Button)
listButtons n = sequence $ V.replicate n $ new Gtk.Button []

initButton :: Handle -> Button -> Text -> IO ()
initButton hdl button label = do
    after button #clicked $ printLabel hdl button
    Gtk.buttonSetLabel button label

labelsToButtons :: Handle -> Vector Text -> Vector Button -> IO ()
labelsToButtons hdl labels buttons = sequence_ $ V.zipWith (initButton hdl) buttons labels

appendTo :: IsWidget w => Box -> w -> IO ()
appendTo = Gtk.boxAppend

appendAllTo :: IsWidget w => Box -> Vector w -> IO ()
appendAllTo keyboard ws = sequence_ $ V.map (appendTo keyboard) ws

applyLayout :: TextLayout -> IO ButtonLayout
applyLayout = sequence . V.map (\l -> listButtons $ V.length l) 

activateApp :: Application -> Handle -> IO ()
activateApp app hdl = do
  keyboard <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  chosenLayout <- qwertyLayout
  lines <- listLines (length chosenLayout)
  letters <- applyLayout $ chosenLayout
  sequence_ $ V.map (uncurry $ labelsToButtons hdl) $ V.zip chosenLayout letters

  appendAllTo keyboard lines
  sequence_ $ V.map (uncurry appendAllTo) $ V.zip lines letters

  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Virtual Keyboard"
                                 , #child := keyboard
                                 ]
  #show w

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "virtual-keyboard.example"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  keyboardToSpellCheckerHdl <- tempFile
  on app #activate $ activateApp app keyboardToSpellCheckerHdl
  Gio.applicationRun app Nothing
  return ()
