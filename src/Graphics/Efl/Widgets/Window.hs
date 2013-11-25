module Graphics.Efl.Widgets.Window (
   createWindow, Window(..), getWindowCanvas
) where

import Graphics.Efl.Widgets.Reactive
import qualified Graphics.Efl.Window as Peer
import qualified Graphics.Efl.Canvas as Peer
import Control.Applicative

data Window = Window {
   windowPeer :: Peer.Window,
   windowSize :: Property (Int,Int),
   windowPosition :: Property (Int,Int),
   windowTitle :: Property String,
   windowVisible :: Property Bool,
   windowFocus :: Property Bool
}


createWindow :: Maybe String -> Int -> Int -> Int -> Int -> Maybe String -> IO Window
createWindow engine x y w h options = do
   peer <- Peer.createWindow engine x y w h options

   win <- Window peer
      <$> newProperty (`Peer.resizeWindow` peer) (Peer.getWindowSize peer)
      <*> newProperty (`Peer.moveWindow` peer) (Peer.getWindowPosition peer)
      <*> newProperty (`Peer.setWindowTitle` peer) (Peer.getWindowTitle peer)
      <*> newProperty (`Peer.setWindowVisible` peer) (Peer.getWindowVisible peer)
      <*> newProperty (`Peer.setWindowFocus` peer) (Peer.getWindowFocus peer)

   Peer.onWindowResize peer $ do
      triggerPropertyEvent (windowSize win)
      triggerPropertyEvent (windowPosition win)
   Peer.onWindowMove peer (triggerPropertyEvent (windowPosition win))
   Peer.onWindowHide peer (triggerPropertyEvent (windowVisible win))
   Peer.onWindowShow peer (triggerPropertyEvent (windowVisible win))
   Peer.onWindowFocusIn peer (triggerPropertyEvent (windowFocus win))
   Peer.onWindowFocusOut peer (triggerPropertyEvent (windowFocus win))

   return win

getWindowCanvas :: Window -> IO Peer.Canvas
getWindowCanvas = Peer.getWindowCanvas . windowPeer
