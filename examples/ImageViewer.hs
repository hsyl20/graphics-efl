{-# Language MultiWayIf,LambdaCase #-}

import Graphics.Efl.Core
import Graphics.Efl.Simple hiding (onEvent)

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit
import Text.Printf

import qualified Data.Vector as Vector
import Data.Vector ((!), Vector)


backgroundColor :: (Int,Int,Int,Int)
backgroundColor = (64,64,64,255)

main :: IO ()
main = do
   withDefaultWindow Nothing $ \win canvas -> do

      bg <- configureBackground win canvas

      images <- Vector.fromList <$> getArgs
      currentImage <- atomically $ newTVar 0

      putStrLn (printf "%d images to show" (Vector.length images))

      when (Vector.length images == 0) $ do
         quitMainLoop
         exitSuccess

      img <- addFilledImage canvas

      showImage win img $ images ! 0

      enableEventPassing img
      uncover img

      let onWindowResizeHandler = do
            (_,_,w,h) <- getWindowGeometry win
            resize (fromIntegral w) (fromIntegral h) bg
            refresh img canvas win

      onWindowResize win onWindowResizeHandler

      onWindowResizeHandler


      flip onKeyDown bg $ \ _ ev -> keyDownKey ev >>= \case 
         "space" -> nextImage win img currentImage images
         "n" -> nextImage win img currentImage images
         "p" -> previousImage win img currentImage images
         "q" -> quitMainLoop
         _ -> return ()

      flip onMouseDown bg $ \ _ _ -> do
         putStrLn "Mouse down"


-- Refresh current display
refresh :: Object -> Canvas -> Window -> IO ()
refresh img _ win = do
   disableMap img

   (_,_,cw,ch) <- getWindowGeometry win
   (iw,ih) <- getImageSize img

   let ratioH = (fromIntegral ch) / (fromIntegral ih)
       ratioW = (fromIntegral cw) / (fromIntegral iw)
       ratio = min 1.0 (min ratioH ratioW)
       w = floor $ (ratio * fromIntegral iw :: Double)
       h = floor $ (ratio * fromIntegral ih :: Double)
       x = floor $ max 0 ((fromIntegral cw - fromIntegral w :: Double) / 2)
       y = floor $ max 0 ((fromIntegral ch - fromIntegral h :: Double) / 2)
  
   resize w h img
   move x y img

-- | Select the next image to show given the current one
selectImage :: (Int -> Int) -> Window -> Object -> TVar Int -> Vector String -> IO ()
selectImage f win img current images = do
   (c,c') <- atomically $ do
      curr <- readTVar current
      let curr' = f curr
      writeTVar current curr'
      return (curr, curr')

   when (c /= c') $ showImage win img (images ! c')


-- Switch to next image
nextImage :: Window -> Object -> TVar Int -> Vector String -> IO ()
nextImage win img current images = selectImage f win img current images
   where f x = if x+1 < Vector.length images then x+1 else x


-- Switch to previous image
previousImage :: Window -> Object -> TVar Int -> Vector String -> IO ()
previousImage = selectImage f
   where  f x = if x > 0 then x-1 else x

-- Show the image whose path is given as a parameter
showImage :: Window -> Object -> String -> IO ()
showImage win img path = do
  canvas <- getCanvas img
  putStrLn (printf "Show image %s" (show path))
  setImageFile path Nothing img
  err <- getImageLoadError img
  case err of
    EvasLoadErrorNone -> return ()
    _ -> putStrLn =<< loadErrorString (fromEnum err)

  refresh img canvas win

-- Configure background with "backgroundColor"
configureBackground :: Window -> Canvas -> IO Object
configureBackground _ canvas = do
  bg <- addRectangle canvas
  setObjectColor backgroundColor bg
  (w,h) <- getCanvasOutputSize canvas
  resize (fromIntegral w) (fromIntegral h) bg
  uncover bg
  setFocus True bg

  return bg
