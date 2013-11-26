module Graphics.Efl.Widgets.Button where

import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Window
import Graphics.Efl.Widgets.Rectangle
import Control.Applicative

data Button = Button {
   buttonBackground :: Rectangle,

   buttonPosition :: Property (Int,Int),
   buttonSize :: Property (Int,Int),
   buttonVisible :: Property Bool,

   buttonPressed :: Property Bool
}


createButton :: Window -> IO Button
createButton win = do

   bg <- createRectangle win

   -- Button pressed automaton
   let 
      unpressed = Auto False [
            rectangleMouseDown bg --> const pressed
         ]
      pressed = Auto True [
            rectangleMouseUp bg --> const unpressed
         ]

   btn <- Button bg
      <$> newIORefProperty (0,0)
      <*> newIORefProperty (40,15)
      <*> newIORefProperty True
      <*> runAutomaton unpressed id

   rectangleSize bg =& readProperty (buttonSize btn)
   rectanglePosition bg =& readProperty (buttonPosition btn)
   rectangleVisible bg =& readProperty (buttonVisible btn)

   rectangleColor bg =& do
      press <- readProperty (buttonPressed btn)
      return $ if press then (200,200,200,255) else (240,240,240,255)

   return btn
