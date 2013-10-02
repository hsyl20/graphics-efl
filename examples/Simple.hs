import Graphics.Efl.Canvas as Canvas
import Graphics.Efl.CoreCanvas as CoreCanvas
import Graphics.Efl.Core
import Foreign.Ptr
import Control.Concurrent

main :: IO ()
main = do
   CoreCanvas.init
   window <- CoreCanvas.new nullPtr 0 0 800 600 nullPtr
   canvas <- CoreCanvas.get window
   CoreCanvas.show window
   
   _ <- forkIO beginMainLoop

   r <- addRectangle canvas
   resize r 100 40
   move r 20 40
   setColor r 255 0 0 255
   Canvas.show r

   t <- addText canvas
   setTextStyle t EvasTextStylePlain EvasTextStyleShadowDirectionBottomRight
   setTextFont t "DejaVu" 14
   resize t 100 50
   move t 25 50
   setColor t 0 255 0 255
   setText t "Haskell-EFL!!"
   Canvas.show t

   return ()
