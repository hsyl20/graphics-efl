import Graphics.Efl.Simple
import Control.Monad (when)
import Control.Applicative
import Data.Foldable (traverse_)

import Graphics.Efl.Core.Idle

import Control.Concurrent.STM
import qualified Data.STM.LinkedList as LL

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

      ll <- atomically $ LL.empty

      let deleteAll xs = do
            e <- LL.start xs
            case e of
               Nothing -> return ()
               Just x  -> LL.delete x >> deleteAll xs

      _ <- addIdleEnterer $ do
         elems <- atomically $ do
            es <- LL.toList ll
            deleteAll ll
            return es

         traverse_ id elems
         return True

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
            _ <- atomically $ do
               let str = "You clicked on " ++ name ++ " with button " ++ show btn
               LL.append (print str) ll
            return ()

      r <- addRectangle canvas
            |> setName "First rectangle"
            |> resize 100 40
            |> move 20 40
            |> setObjectColor (255,0,0,255)
            |> uncover
            |> onMouseDown clickHandler

      return ()
