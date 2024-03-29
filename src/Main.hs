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


import Data.Text (Text, pack)
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
import Data.Aeson (Array, Value(Object, String, Array), decode)
import Data.Vector (Vector)
import Data.Aeson.KeyMap (Key)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import qualified Data.Text as T (head)
import qualified Data.Vector as V (replicate, map, zipWith, length, singleton, zip)
import qualified Data.Aeson.KeyMap as KM ((!?))
import qualified Data.ByteString.Lazy as B (readFile)

import Codec.Serialise (readFileDeserialise)

import GI.Gtk (Button, Box, IsWidget, Application)
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base (new, AttrOp((:=)), after, on)

import SpellCheckerInterface (completeWord, correctWord)
import Data.WordTree (fromMap, CountedWord(word), Tree)

type TextLayout = Vector (Vector Text)
type ButtonLayout = Vector (Vector Gtk.Button)
type Pipe = IORef String
-- ^ IORef (Keyboard -> Spell-checker)

-- | Refreshs the keyboard's suggestion output 
refreshOutput :: (Pipe, Tree Char) -> Button -> IO ()
refreshOutput (pipe, dict) ks = do
  pipeOutput <- reverse <$> readIORef pipe
  let suggestion = listToMaybe $ map word $ completeWord dict pipeOutput
  let keyboardOutput = pack $ fromMaybe pipeOutput suggestion
  Gtk.buttonSetLabel ks $ keyboardOutput

-- | Prints hitted button to stdout
sendToPipe :: (Pipe, Tree Char, Button) -> Button -> IO ()
sendToPipe (pipe, dict, ks) b = do
    label <- Gtk.buttonGetLabel b
    case label of
      Nothing -> return ()
      Just c -> modifyIORef' pipe (T.head c :)
    refreshOutput (pipe, dict) ks

-- | Clears the pipe for keyboard's output
clearPipe :: Pipe -> Button -> IO ()
clearPipe pipe ks = do
  writeIORef pipe ""
  Gtk.buttonSetLabel ks $ (pack . reverse) ""

-- | Extracts the text value out of a json
getFromJSON :: Key -> Value -> Text
getFromJSON key (Object o) = (\(String s) -> s) . fromJust $ o KM.!? key

-- | Fetches and maps the qwerty layout as a matrix (TextLayout)
qwertyLayout :: IO TextLayout
qwertyLayout = do
  layoutB <- B.readFile "spell-checker/layouts/qwerty.json"
  let layoutArray = fromJust (decode layoutB :: Maybe Array)
  let fromJSON = V.map (\(Array v) -> V.map (getFromJSON "label") v) layoutArray
  return fromJSON

-- | Adds n rows to the GUI's window
listRows :: Int -> IO (Vector Box)
listRows n = sequence $ V.replicate n $ new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

-- | Adds buttons to the GUI's window (applies to the previously created row)
listButtons :: Int -> IO (Vector Button)
listButtons n = sequence $ V.replicate n $ new Gtk.Button []

-- | Links a button to the associated label
initButton :: (Pipe, Tree Char, Button) -> Button -> Text -> IO ()
initButton pipe b label = do
    after b #clicked $ sendToPipe pipe b
    Gtk.buttonSetLabel b label

-- | Associates labels and buttons together
labelsToButtons :: (Pipe, Tree Char, Button) -> Vector Text -> Vector Button -> IO ()
labelsToButtons pipe labels bs = sequence_ $ V.zipWith (initButton pipe) bs labels

-- | Draws (appends) a list of widgets to the keyboard 
drawOn :: IsWidget w => Box -> Vector w -> IO ()
drawOn keyboard ws = sequence_ $ V.map (Gtk.boxAppend keyboard) ws

-- | Transforms a matrix of labels into a matrix of GUI buttons
applyLayout :: TextLayout -> IO ButtonLayout
applyLayout = sequence . V.map (\l -> listButtons $ V.length l) 

-- | Draws the app GUI
activateApp :: Pipe -> Tree Char -> Application -> IO ()
activateApp pipe dict app = do
  keyboard <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  
  keyboardSuggestion <- new Gtk.Button []
  after keyboardSuggestion #clicked $ clearPipe pipe keyboardSuggestion
  drawOn keyboard $ V.singleton keyboardSuggestion

  chosenLayout <- qwertyLayout
  lines <- listRows (length chosenLayout)
  letters <- applyLayout $ chosenLayout
  sequence_ $ V.map (uncurry $ labelsToButtons (pipe, dict, keyboardSuggestion)) $ V.zip chosenLayout letters

  drawOn keyboard lines
  sequence_ $ V.map (uncurry drawOn) $ V.zip lines letters

  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Virtual Keyboard"
                                 , #child := keyboard
                                 ]
  #show w

main :: IO ()
main = do
  pipe <- newIORef "" -- Pipe Keyboard <-> Spell checker
  inputFreq <- readFileDeserialise "spell-checker/SerializedStatistics/result" 
  let dictionaryTree = fromMap inputFreq
  app <- new Gtk.Application [ #applicationId := "virtual-keyboard.example"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  on app #activate $ activateApp pipe dictionaryTree app
  Gio.applicationRun app Nothing
  return ()
