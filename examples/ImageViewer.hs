{-# Language MultiWayIf,LambdaCase #-}

import Graphics.Efl.Core
import Graphics.Efl.Simple

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad (void)
import Foreign.Ptr
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

      if Vector.length images == 0 then (quitMainLoop >> exitSuccess) else return ()

      img <- addFilledImage canvas

      showImage win img $ images ! 0

      enableEventPassing img
      uncover img

      let onWindowResizeHandler = do
            (_,_,w,h) <- getWindowGeometry win
            resize w h bg
            refresh img canvas win

      onWindowResize win onWindowResizeHandler

      onWindowResizeHandler


      onKeyDown bg $ \case 
         "space" -> nextImage win img currentImage images
         "n" -> nextImage win img currentImage images
         "p" -> previousImage win img currentImage images
         "t" -> rotate img 90.0
         "q" -> quitMainLoop
         _ -> return ()

      onMouseDown bg $ do
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
  
   putStrLn (show [cw,ch,iw,ih,x,y,w,h])
   resize w h img
   move x y img

--   tr <- createMap 4
--            # populateMapPointsFromGeometry x y w h 1
--   setMap tr img
--   enableMap img
--   freeMap tr



rotate :: Object -> Double -> IO ()
rotate img angle = do
   (x,y,w,h) <- getGeometry img
   tr <- dupMap =<< getMap img
   rotateMap angle (x + w `div` 2) (y + h `div` 2) tr
   setMap tr img
   enableMap img
   freeMap tr

-- Switch to next image
nextImage :: Window -> Object -> TVar Int -> Vector String -> IO ()
nextImage win img current images = do
   let f x = if x+1 < Vector.length images then x+1 else x

   c <- atomically $ do
      modifyTVar current f
      readTVar current
   showImage win img (images ! c)

-- Switch to previous image
previousImage :: Window -> Object -> TVar Int -> Vector String -> IO ()
previousImage win img current images = do
   let f x = if x > 0 then x-1 else x

   c <- atomically $ do
      modifyTVar current f
      readTVar current

   showImage win img (images ! c)

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

onMouseDown :: Object -> IO () -> IO ()
onMouseDown = onEvent EvasCallbackMouseDown

onKeyDown :: Object -> (String -> IO ()) -> IO ()
onKeyDown obj cb = do 
  wcb <- wrapEventCallback $ \_ _ _ info -> do
    keyName <- keyDownKey info
    cb keyName
  void $ addEventCallback obj EvasCallbackKeyDown wcb nullPtr
  
onEvent :: CallbackType -> Object -> IO () -> IO ()
onEvent evType obj cb = do
  wcb <- wrapEventCallback $ \_ _ _ _ -> cb
  void $ addEventCallback obj evType wcb nullPtr

-- Configure background with "backgroundColor"
configureBackground :: Window -> Canvas -> IO Object
configureBackground _ canvas = do
  bg <- addRectangle canvas
  let (red, green, blue, alpha) = backgroundColor
  setColor red green blue alpha bg
  (w,h) <- getCanvasOutputSize canvas
  resize w h bg
  uncover bg
  setFocus True bg

  return bg
