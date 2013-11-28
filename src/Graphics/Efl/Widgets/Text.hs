module Graphics.Efl.Widgets.Text where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Window
import Graphics.Efl.Widgets.Widget
import qualified Graphics.Efl.Canvas as Peer
import Control.Applicative
import Control.Monad.IO.Class

type Color = (Int,Int,Int,Int)

data Text = Text {
   textPeer :: Peer.Object,
   textSize :: Property (Int,Int),
   textPosition :: Property (Int,Int),
   textVisible :: Property Bool,
   textFocus :: Property Bool,
   textColor :: Property Color,
   textPassEvents :: Property Bool,

   textString :: Property String,
   textStyle :: Property (Peer.TextStyle,Peer.TextShadowStyle),
   textFont :: Property (String,Peer.FontSize),
   textStylePad :: Property (Int,Int,Int,Int),

   textMouseDown :: Signal Peer.MouseDownEvent,
   textMouseUp :: Signal Peer.MouseUpEvent
}

instance Widget Text where
   widgetSize = textSize
   widgetPosition = textPosition
   widgetVisible = textVisible

createText :: Window -> IO Text
createText win = do
   peer <- Peer.addText =<< getWindowCanvas win

   rect <- Text peer
      <$> newProperty (`Peer.setObjectSize` peer) (Peer.getObjectSize peer)
      <*> newProperty (`Peer.setObjectPosition` peer) (Peer.getObjectPosition peer)
      <*> newProperty (`Peer.setObjectVisible` peer) (Peer.getObjectVisible peer)
      <*> newProperty (`Peer.setObjectFocus` peer) (Peer.getObjectFocus peer)
      <*> newProperty (`Peer.setObjectColor` peer) (Peer.getObjectColor peer)
      <*> newProperty (`Peer.setObjectPassEvents` peer) (Peer.getObjectPassEvents peer)
      <*> newProperty (`Peer.setText` peer) (Peer.getText peer)
      <*> newProperty (`Peer.setTextStyle` peer) (Peer.getTextStyle peer)
      <*> newProperty (`Peer.setTextFont` peer) (Peer.getTextFont peer)
      <*> newIORefProperty (0,0,0,0)
      <*> newSignal
      <*> newSignal

   Peer.onObjectResize peer $ do
      triggerPropertyEvent (textSize rect)
      triggerPropertyEvent (textPosition rect)
   Peer.onObjectMove peer (triggerPropertyEvent (textPosition rect))
   Peer.onObjectHide peer (triggerPropertyEvent (textVisible rect))
   Peer.onObjectShow peer (triggerPropertyEvent (textVisible rect))
   Peer.onObjectFocusIn (triggerPropertyEvent (textFocus rect)) peer
   Peer.onObjectFocusOut (triggerPropertyEvent (textFocus rect)) peer 

   textStylePad rect =& do
      _ <- readProperty (textStyle rect)
      _ <- readProperty (textFont rect)
      _ <- readProperty (textString rect)
      liftIO $ Peer.getTextStylePad peer

   Peer.onMouseDown (\_ info -> triggerSignal (textMouseDown rect) info) peer
   Peer.onMouseUp (\_ info -> triggerSignal (textMouseUp rect) info) peer

   return rect
