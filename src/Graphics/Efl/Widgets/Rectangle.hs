module Graphics.Efl.Widgets.Rectangle where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Window
import qualified Graphics.Efl.Canvas as Peer
import Control.Applicative

type Color = (Int,Int,Int,Int)

data Rectangle = Rectangle {
   rectanglePeer :: Peer.Object,
   rectangleSize :: Property (Int,Int),
   rectanglePosition :: Property (Int,Int),
   rectangleVisible :: Property Bool,
   rectangleFocus :: Property Bool,
   rectangleColor :: Property Color,

   rectangleMouseDown :: Signal Peer.MouseDownEvent,
   rectangleMouseUp :: Signal Peer.MouseUpEvent
}

createRectangle :: Window -> IO Rectangle
createRectangle win = do
   peer <- Peer.addRectangle =<< getWindowCanvas win

   rect <- Rectangle peer
      <$> newProperty (`Peer.setObjectSize` peer) (Peer.getObjectSize peer)
      <*> newProperty (`Peer.setObjectPosition` peer) (Peer.getObjectPosition peer)
      <*> newProperty (`Peer.setObjectVisible` peer) (Peer.getObjectVisible peer)
      <*> newProperty (`Peer.setObjectFocus` peer) (Peer.getObjectFocus peer)
      <*> newProperty (`Peer.setObjectColor` peer) (Peer.getObjectColor peer)
      <*> newSignal
      <*> newSignal

   Peer.onObjectResize peer $ do
      triggerPropertyEvent (rectangleSize rect)
      triggerPropertyEvent (rectanglePosition rect)
   Peer.onObjectMove peer (triggerPropertyEvent (rectanglePosition rect))
   Peer.onObjectHide peer (triggerPropertyEvent (rectangleVisible rect))
   Peer.onObjectShow peer (triggerPropertyEvent (rectangleVisible rect))
   Peer.onObjectFocusIn peer (triggerPropertyEvent (rectangleFocus rect))
   Peer.onObjectFocusOut peer (triggerPropertyEvent (rectangleFocus rect))

   Peer.onMouseDown (\_ info -> triggerSignal (rectangleMouseDown rect) info) peer
   Peer.onMouseUp (\_ info -> triggerSignal (rectangleMouseUp rect) info) peer

   return rect
