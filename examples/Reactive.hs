import Graphics.Efl.Simple
import Control.Monad (when)
import Control.Applicative

import Graphics.Efl.Core.Idle
import Graphics.Efl.Eina

import Control.Concurrent.STM

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

      addIdleEnterer (putStrLn "Kikou" >> return (fromBool True))

      inTxt <- addText canvas
            |> setText "Your text: "
            |> resize 200 10
            |> move 25 200
            |> setTextStyle (TextStylePlain,TextStyleShadowDirectionBottomRight)
            |> setTextFont ("DejaVu",14)
            |> setObjectColor (255,255,255,255)
            |> enableEventPassing
            |> uncover


      bg <- addRectangle canvas
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

      return ()
