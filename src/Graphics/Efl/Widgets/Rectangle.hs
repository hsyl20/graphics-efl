module Graphics.Efl.Widgets.Rectangle where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Window
import Graphics.Efl.Widgets.Widget
import Graphics.Efl.Widgets.BasicObject
import qualified Graphics.Efl.Canvas as Peer

type Color = (Int,Int,Int,Int)

data Rectangle = Rectangle {
   rectanglePeer :: Peer.Object,
   rectangleSize :: Property (Int,Int),
   rectanglePosition :: Property (Int,Int),
   rectangleVisible :: Property Bool,
   rectangleFocused :: Property Bool,
   rectangleColor :: Property Color,

   rectangleMouseDown :: Signal Peer.MouseDownEvent,
   rectangleMouseUp :: Signal Peer.MouseUpEvent,
   rectangleMouseIn :: Signal Peer.MouseInEvent,
   rectangleMouseOut :: Signal Peer.MouseOutEvent,
   rectangleMouseWheel :: Signal Peer.MouseWheelEvent,
   rectangleMouseMove :: Signal Peer.MouseMoveEvent,
   rectangleMultiDown :: Signal Peer.MultiDownEvent,
   rectangleMultiUp :: Signal Peer.MultiUpEvent,
   rectangleMultiMove :: Signal Peer.MultiMoveEvent,
   rectangleFree :: Signal (),
   rectangleKeyDown :: Signal Peer.KeyDownEvent,
   rectangleKeyUp :: Signal Peer.KeyUpEvent
}

instance BasicObject Rectangle where
   peerObject = rectanglePeer
   onMouseDown = rectangleMouseDown
   onMouseUp = rectangleMouseUp
   onMouseIn = rectangleMouseIn
   onMouseOut = rectangleMouseOut
   onMouseWheel = rectangleMouseWheel
   onMouseMove = rectangleMouseMove
   onMultiDown = rectangleMultiDown
   onMultiUp = rectangleMultiUp
   onMultiMove = rectangleMultiMove
   onFree = rectangleFree
   onKeyDown = rectangleKeyDown
   onKeyUp = rectangleKeyUp
   isFocused = rectangleFocused

instance Widget Rectangle where
   widgetSize = rectangleSize
   widgetPosition = rectanglePosition
   widgetVisible = rectangleVisible

createRectangle :: Window -> IO Rectangle
createRectangle win = do
   peer <- Peer.addRectangle =<< getWindowCanvas win

   rect <- Rectangle peer
      <$> newProperty (`Peer.setObjectSize` peer) (Peer.getObjectSize peer)
      <*> newProperty (`Peer.setObjectPosition` peer) (Peer.getObjectPosition peer)
      <*> newProperty (`Peer.setObjectVisible` peer) (Peer.getObjectVisible peer)
      <*> newProperty (`Peer.setObjectFocus` peer) (Peer.getObjectFocus peer)
      <*> newProperty (`Peer.setObjectColor` peer) (Peer.getObjectColor peer)
      <*> newSignal <*> newSignal <*> newSignal <*> newSignal <*> newSignal
      <*> newSignal <*> newSignal <*> newSignal <*> newSignal <*> newSignal
      <*> newSignal <*> newSignal

   initBasicObject rect

   Peer.onObjectResize peer $ do
      triggerPropertyEvent (rectangleSize rect)
      triggerPropertyEvent (rectanglePosition rect)
   Peer.onObjectMove peer (triggerPropertyEvent (rectanglePosition rect))
   Peer.onObjectHide peer (triggerPropertyEvent (rectangleVisible rect))
   Peer.onObjectShow peer (triggerPropertyEvent (rectangleVisible rect))

   return rect
