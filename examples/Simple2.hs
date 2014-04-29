{-# LANGUAGE BangPatterns,TupleSections #-}
import Graphics.Efl.Widgets.Window 
import Graphics.Efl.Widgets.Rectangle
import Graphics.Efl.Widgets.Button
import Graphics.Efl.Widgets.Reactive
import Graphics.Efl.Widgets.Text
import Graphics.Efl.Widgets.BasicObject
import Graphics.Efl.Canvas.MouseEvents
import Graphics.Efl.Canvas.Types (Point(..))
import Graphics.Efl.Simple(initWindowingSystem, beginMainLoop, getEngines, createJob)
import Text.Printf
import System.IO.Unsafe
import Control.Applicative

main :: IO ()
main = do
   initWindowingSystem

   engines <- getEngines

   let engine = if "opengl_x11" `elem` engines 
         then Just "opengl_x11"
         else Nothing

   _ <- createJob $ do
      win <- createWindow engine 0 0 800 600 Nothing

      windowVisible win =& return True
      windowTitle win =& do
         (w,h) <- readProperty (windowSize win)
         (x,y) <- readProperty (windowPosition win)
         return $ printf "Reactive EFL: (%d,%d,%d,%d)" x y w h


      bg <- createRectangle win
      rectangleVisible bg =& return True
      rectanglePosition bg =& return (0,0)
      rectangleColor bg =& do
         (w,h) <- readProperty (windowSize win)
         let f x = floor ((fromIntegral x / 2000.0) * 255.0 :: Double) `mod` 255
         return (f w, f h,0,255)
      rectangleSize bg =& readProperty (windowSize win)

      btn <- createButton win
      buttonPosition btn =& return (200,100)
      buttonSize btn =& return (200,80)
      buttonText btn =& do
         press <- readProperty (buttonPressed btn)
         return $ if press then "Clicked!" else "Click?"

      textFont (buttonLabel btn) =& do
         press <- readProperty (buttonPressed btn)
         return $ if press then ("Comic sans ms", 20) else ("DejaVu", 16)

      incbtn <- createButton win
      buttonPosition incbtn =& return (50,50)
      buttonSize incbtn =& return (50,50)
      -- Counter automaton
      let counterAutomaton :: Auto Int
          counterAutomaton = Auto [ onMouseDown (buttonBackground incbtn) -@> const (+1) ]
      counterProp <- runAutomaton counterAutomaton show 0
      buttonText incbtn =& readProperty counterProp

      movebtn <- createButton win
      -- Move automaton
      let static = Auto [ onMouseDown (buttonBackground movebtn) --> const (,moving) ]
          moving = Auto [
               onMouseMove (buttonBackground movebtn) -@> \ info ->
                  let
                     Point !oldx !oldy = unsafePerformIO $ mouseMovePreviousCanvasXY info
                     Point !newx !newy = unsafePerformIO $ mouseMoveCanvasXY info
                  in \(nx,ny) -> (nx+newx-oldx, ny+newy-oldy),
               onMouseUp (buttonBackground movebtn) --> const (,static)
             ]
      positionProp <- runAutomaton static id (100,50)

      buttonPosition movebtn =& readProperty positionProp
      buttonSize movebtn =& return (150,50)
      buttonText movebtn =& return "Click to move!"

      txt <- createText win
      textPosition txt =& return (200,200)
      textString txt =& (show <$> readProperty positionProp)
      textFont txt =& return ("DejaVu", 16)
      textVisible txt =& return True

   beginMainLoop
   return ()
