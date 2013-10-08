import Graphics.Efl.Simple

main :: IO ()
main = do
   initWindowingSystem
   engines <- getEngines
   putStrLn (show engines)

   withDefaultWindow (Just "opengl_x11") $ \ win canvas -> do
      engineName <- getEngineName win
      putStrLn $ "Using engine " ++ engineName

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

      r2 <- addRectangle canvas
            # resize 100 40
            # move 100 100
            # setColor 128 128 0 255
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
            

      ator <- createAnimator
               # setAnimatorFrameRate 60

      (cx,cy) <- center r

      addAnimationLinear ator (seconds 4) Nothing $ \step -> do
         m <- createMap 4
               # populateMapPointsFromObject r
               # rotateMap (360.0 * step) cx cy
         setMap m r
         enableMap r

      addAnimationLinear ator (seconds 0.5) Nothing $ \step -> do
         (cx',cy') <- center r2
         m <- createMap 4
               # populateMapPointsFromObject r2
               # rotateMap (360.0 * step) cx' cy'
         setMap m r2
         enableMap r2

      addAnimationBounce ator (seconds 1) (Just 2) $ \step -> do
         let x'' = (100+(floor $ step * 200))
         move x'' 100 r2
