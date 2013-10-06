import Graphics.Efl.Simple

main :: IO ()
main = do
   withDefaultWindow $ \ _ canvas -> do

      r <- addRectangle canvas
            # resize 100 40
            # move 20 40
            # setColor 255 0 0 255
            # uncover

      t <- addText canvas
            # setText "Haskell-EFL!!"
            # resize 200 10
            # move 25 50
            # setTextStyle EvasTextStylePlain EvasTextStyleShadowDirectionBottomRight
            # setTextFont "DejaVu" 14
            # setColor 0 255 0 255
            # uncover

      renderCanvas canvas

      let center o = do
            (x,y,w,h) <- getGeometry o
            return (w `div` 2 + x, h `div` 2 + y)
            

      (cx,cy) <- center t

      m <- createMap 4
            # populateMapPointsFromObject r
            # rotateMap 5 cx cy

      setMap m r
      enableMap r

      return ()
