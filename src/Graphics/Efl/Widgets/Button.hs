module Graphics.Efl.Widgets.Button where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Window
import Graphics.Efl.Widgets.Rectangle
import Graphics.Efl.Widgets.Text
import Graphics.Efl.Widgets.Widget
import Graphics.Efl.Widgets.Layout
import qualified Graphics.Efl.Canvas as Peer
import Control.Applicative

data Button = Button {
   buttonBackground :: Rectangle,
   buttonLabel :: Text,

   buttonPosition :: Property (Int,Int),
   buttonSize :: Property (Int,Int),
   buttonVisible :: Property Bool,
   buttonText :: Property String,

   buttonPressed :: Property Bool
}

instance Widget Button where
   widgetSize = buttonSize
   widgetPosition = buttonPosition
   widgetVisible = buttonVisible


createButton :: Window -> IO Button
createButton win = do

   bg <- createRectangle win
   label <- createText win

   -- Button pressed automaton
   let 
      unpressed = Auto False [
            rectangleMouseDown bg --> const (const pressed)
         ]
      pressed = Auto True [
            rectangleMouseUp bg --> const (const unpressed)
         ]

   btn <- Button bg label
      <$> newIORefProperty (0,0)
      <*> newIORefProperty (40,15)
      <*> newIORefProperty True
      <*> newIORefProperty "Press me"
      <*> runAutomaton unpressed id

   rectangleSize bg =& readProperty (buttonSize btn)
   rectanglePosition bg =& readProperty (buttonPosition btn)
   rectangleVisible bg =& readProperty (buttonVisible btn)

   textString label =& readProperty (buttonText btn)
   textVisible label =& readProperty (buttonVisible btn)
   label `alignCenter` btn

   textPassEvents label =& return True
   textStyle label =& return (Peer.TextStylePlain, Peer.TextStyleShadowDirectionBottomRight)
   textFont label =& return ("DejaVu", 14)
   textColor label =& return (50,50,50,255)

   rectangleColor bg =& do
      press <- readProperty (buttonPressed btn)
      return $ if press then (200,200,200,255) else (240,240,240,255)

   return btn
