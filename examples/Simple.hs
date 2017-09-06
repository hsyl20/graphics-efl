import Graphics.Efl.Simple
import Control.Monad (when)
import Paths_graphics_efl

main :: IO ()
main = do
   initWindowingSystem
   engines <- getEngines
   putStrLn (show engines)

   let engine = if "opengl_x11" `elem` engines
         then Just "opengl_x11"
         else Nothing

   withDefaultWindow engine $ \ win canvas -> do
      engineName <- getEngineName win
      putStrLn $ "Using engine " ++ engineName

      setWindowTitle "Simple Haskell-EFL Example" win

      bgfile <- getDataFileName "examples/data/Red_Giant_Earth_warm.jpg"

      inTxt <- addText canvas
            |> setText "Your text: "
            |> resize 200 10
            |> move 25 200
            |> setTextStyle (TextStylePlain,TextStyleShadowDirectionBottomRight)
            |> setTextFont ("DejaVu",14)
            |> setObjectColor (255,255,255,255)
            |> enableEventPassing
            |> uncover


      -- bg <- addFilledImage canvas
      bg <- addRectangle canvas
            |> setImageFile bgfile Nothing
            |> setObjectColor (0,0,0,255)
            |> setLayer (-1)
            |> uncover
            |> setFocus True
            |> onKeyDown (\ _ ev -> do
                  name <- keyDownKeyName ev
                  old <- getText inTxt
                  new <- case name of
                     "Escape" -> quitMainLoop >> return old
                     "BackSpace" -> return $ if null old then old else init old
                     _ -> (old ++) <$> keyDownString ev
                  setText new inTxt
                  )

      onWindowResize win $ do
         (_,_,w,h) <- getWindowGeometry win
         resize w h bg

      let clickHandler obj ev = do
            name <- getName obj
            btn <- mouseDownButton ev
            putStrLn $ "You clicked on " ++ name ++ " with button " ++ show btn

      r <- addRectangle canvas
            |> setName "First rectangle"
            |> resize 100 40
            |> move 20 40
            |> setObjectColor (255,0,0,255)
            |> uncover
            |> onMouseDown clickHandler
            |> onMouseMove (\_ _ -> putStrLn "Move!")

      r2 <- addRectangle canvas
            |> setName "Second rectangle"
            |> resize 100 40
            |> move 100 100
            |> setObjectColor (128,128,0,128)
            |> uncover
            |> onMouseIn (\_ ev -> do
                  wxy <- mouseInWorldXY ev
                  cxy <- mouseInCanvasXY ev
                  t <- mouseInTimestamp ev
                  putStrLn (show wxy ++ ";" ++ show cxy ++ ";" ++ show t))
            |> onMouseDown clickHandler
            |> onMouseOut (\ _ _-> putStrLn "Out!")
            |> onMouseUp (\ _ _-> putStrLn "Up!")
            |> onMouseWheel (\ _ _-> putStrLn "Wheel!")

      let
         poly :: Int -> Double -> [(Coord,Coord)]
         poly n w = [(round $ x*w, round $ y*w) | (x,y) <- (map cos angles `zip` map sin angles)]
            where
               angles :: [Double]
               angles = map ((*c) . fromIntegral) [0..(n-1)]
               c = 2.0 * pi / fromIntegral n

      po <- addPolygon canvas
            |> addPolygonPoints (poly 8 10)
            |> setObjectColor (128,128,128,255)
            |> move 200 200
            |> uncover

      style <- createTextBlockStyle
            |> configureTextBlockStyle "DEFAULT='font=DejaVuSans-Bold font_size=26 align=center color=#000000 wrap=word style=soft_outline outline_color=#3779cb' NewLine= '+\n'"


      _ <- addTextBlock canvas
            |> setTextBlockStyle style
            |> setTextBlockTextMarkup "Welcome to the <b>Haskell-EFL</b> demo!!!"
            |> resize 500 400
            |> move 300 10
            |> setObjectColor (255,255,255,255)
            |> enableEventPassing
            |> uncover


      let lipsum = "In this demo, you can:<br/>\
                  \  - click on objects (with any button)<br/>\
                  \  - scroll this text with the mouse wheel<br/>\
                  \  - enter some text<br/>\
                  \<br/><br/>\
                  \Press \"Escape\" to quit\
                  \"

      style2 <- createTextBlockStyle
                  |> configureTextBlockStyle "DEFAULT='font=DejaVuSans-Bold font_size=20 align=left color=#000000 wrap=word style=soft_shadow shadow_color=#CCCCCC' NewLine= '+\n'"

      lipsumTxt <- addTextBlock canvas
            |> setTextBlockStyle style2
            |> setTextBlockTextMarkup lipsum
            |> resize 500 400
            |> move 20 400
            |> enableEventPassing
            |> uncover

      flip onMouseWheel bg $ \ _ ev -> do
         dir <- mouseWheelDirection ev
         off <- mouseWheelOffset ev
         (x,y,_,_) <- getGeometry lipsumTxt
         when (dir == 0) $ move x (y + fromIntegral (-10 * off)) lipsumTxt
         when (dir == 1) $ move (x + fromIntegral (10 * off)) y lipsumTxt

      let center o = do
            (x,y,w,h) <- getGeometry o
            return (w `div` 2 + x, h `div` 2 + y)

      --setAnimatorFrameRate 4

      (cx,cy) <- center r

      addAnimationLinear' 1.0 Nothing $ \step -> do
         m <- createMap 4
               |> populateMapPointsFromObject r
               |> rotateMap (360.0 * step) (fromIntegral cx) (fromIntegral cy)
         setMap m r
         enableMap r

      anim <- addAnimationLinear 4.0 Nothing $ \step -> do
         (cx',cy') <- center r2
         m <- createMap 4
               |> populateMapPointsFromObject r2
               |> rotateMap (360.0 * step) (fromIntegral cx') (fromIntegral cy')
         setMap m r2
         enableMap r2

      flip onMouseDown r2 $ \ _ _ -> freezeAnimator anim
      flip onMouseUp r2 $ \ _ _ -> thawAnimator anim

      addAnimationLinear' 3.0 (Just 1) $ \step -> do
         let x' = (100+(floor $ step * 200))
         move x' 100 r2

      addAnimationBounce' 4 Nothing $ \step -> do
         let x' = (100+(floor $ step * 400))
         (_,y,_,_) <- getGeometry po
         move x' y po

      addAnimationSinusoidal' 6 8.0 Nothing $ \step -> do
         let y' = (200+(floor $ step * 100))
         (x,_,_,_) <- getGeometry po
         move x y' po

      return ()
