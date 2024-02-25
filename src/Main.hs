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
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T (head)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM ((!?))
import qualified Data.ByteString.Lazy as B (readFile)

import GI.Gtk (Button, Box, IsWidget, Application)
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import Data.GI.Base (new, AttrOp((:=)), after, on)

type TextLayout = V.Vector (V.Vector Text)
type ButtonLayout = V.Vector (V.Vector Gtk.Button)
type Pipe = IORef String

printLabel :: Pipe -> Button -> IO ()
printLabel pipe b = do
    label <- Gtk.buttonGetLabel b
    case label of
      Nothing -> return ()
      Just c -> modifyIORef' pipe (\s -> T.head c : s)
    s <- readIORef pipe
    print s
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

initButton :: Pipe -> Button -> Text -> IO ()
initButton pipe b label = do
    after b #clicked $ printLabel pipe b
    Gtk.buttonSetLabel b label

labelsToButtons :: Pipe -> Vector Text -> Vector Button -> IO ()
labelsToButtons pipe labels bs = sequence_ $ V.zipWith (initButton pipe) bs labels

appendTo :: IsWidget w => Box -> w -> IO ()
appendTo = Gtk.boxAppend

appendAllTo :: IsWidget w => Box -> Vector w -> IO ()
appendAllTo keyboard ws = sequence_ $ V.map (appendTo keyboard) ws

applyLayout :: TextLayout -> IO ButtonLayout
applyLayout = sequence . V.map (\l -> listButtons $ V.length l) 

activateApp :: Pipe -> Application -> IO ()
activateApp pipe app = do
  keyboard <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  chosenLayout <- qwertyLayout
  lines <- listLines (length chosenLayout)
  letters <- applyLayout $ chosenLayout
  sequence_ $ V.map (uncurry $ labelsToButtons pipe) $ V.zip chosenLayout letters

  appendAllTo keyboard lines
  sequence_ $ V.map (uncurry appendAllTo) $ V.zip lines letters

  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Virtual Keyboard"
                                 , #child := keyboard
                                 ]
  #show w

main :: IO ()
main = do
  pipe <- newIORef "" -- Pipe Keyboard <-> Spell checker
  app <- new Gtk.Application [ #applicationId := "virtual-keyboard.example"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  on app #activate $ activateApp pipe app
  Gio.applicationRun app Nothing
  return ()
