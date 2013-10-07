import Graphics.Efl.Simple

import Control.Concurrent
import Control.Applicative

main :: IO ()
main = do
   initWindowingSystem
   engines <- getEngines
   putStrLn (show engines)

   withDefaultWindow (Just "opengl_x11") $ \ win canvas -> do
      putStrLn =<< ("Using engine " ++) <$> getEngineName win

      setWindowTitle "Simple Haskell-EFL Example" win

      bg <- addRectangle canvas
            # setColor 0 0 0 255
            # uncover

      onWindowResize win $ do
         (_,_,w,h) <- getWindowGeometry win
         resize w h bg

      r <- addRectangle canvas
            # resize 100 40
            # move 20 40
            # setColor 255 0 0 255
            # uncover

      _ <- addText canvas
            # setText "Haskell-EFL!!"
            # resize 200 10
            # move 25 50
            # setTextStyle TextStylePlain TextStyleShadowDirectionBottomRight
            # setTextFont "DejaVu" 14
            # setColor 0 255 0 255
            # uncover

      let center o = do
            (x,y,w,h) <- getGeometry o
            return (w `div` 2 + x, h `div` 2 + y)
            
      (cx,cy) <- center r
      m <- createMap 4
            # populateMapPointsFromObject r
      setMap m r
      enableMap r

      let run = do
               rotateMap 5 cx cy m
               setMap m r
               threadDelay (1000 * 100)
               run

      run
