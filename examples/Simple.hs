import Graphics.Efl.Canvas
import Graphics.Efl.Window
import Graphics.Efl.Core
import Control.Concurrent

main :: IO ()
main = do
   initWindowingSystem
   window <- createWindow Nothing 0 0 800 600 Nothing
   canvas <- getWindowCanvas window
   showWindow window
   
   _ <- forkIO beginMainLoop

   r <- addRectangle canvas
   resize r 100 40
   move r 20 40
   setColor r 255 0 0 255
   uncover r

   t <- addText canvas
   setTextStyle t EvasTextStylePlain EvasTextStyleShadowDirectionBottomRight
   setTextFont t "DejaVu" 14
   resize t 100 50
   move t 25 50
   setColor t 0 255 0 255
   setText t "Haskell-EFL!!"
   uncover t

   return ()
