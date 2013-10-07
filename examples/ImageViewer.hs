{-# Language MultiWayIf,LambdaCase #-}

import Graphics.Efl.Core
import Graphics.Efl.Simple

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad (void)
import Foreign.C.String
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

      showImage img $ images ! 0

      enableEventPassing img
      uncover img

      onWindowResize win $ do
         (_,_,w,h) <- getWindowGeometry win
         resize w h bg
         --refresh img canvas


      tr <- createMap 4
               # populateMapPointsFromObject img
      setMap tr img
      enableMap img
      freeMap tr

      zoomFit img canvas


      onKeyDown bg $ \case 
         "space" -> nextImage img currentImage images
         "n" -> nextImage img currentImage images
         "p" -> previousImage img currentImage images
         "t" -> rotate img 90.0
         "q" -> quitMainLoop
         _ -> return ()

      onMouseDown bg $ do
         putStrLn "Mouse down"

-- Refresh current display
refresh :: Object -> Canvas -> IO ()
refresh img canvas = do
  zoomFit img canvas
  centerImage img canvas


rotate :: Object -> Double -> IO ()
rotate img angle = do
   (x,y,w,h) <- getGeometry img
   tr <- dupMap =<< getMap img
   rotateMap angle (x + w `div` 2) (y + h `div` 2) tr
   setMap tr img
   enableMap img
   freeMap tr

-- Switch to next image
nextImage :: Object -> TVar Int -> Vector String -> IO ()
nextImage img current images = do
   let f x = if x+1 < Vector.length images then x+1 else x

   c <- atomically $ do
      modifyTVar current f
      readTVar current
   showImage img (images ! c)

-- Switch to previous image
previousImage :: Object -> TVar Int -> Vector String -> IO ()
previousImage img current images = do
   let f x = if x > 0 then x-1 else x

   c <- atomically $ do
      modifyTVar current f
      readTVar current

   showImage img (images ! c)

-- Show the image whose path is given as a parameter
showImage :: Object -> String -> IO ()
showImage img path = do
  canvas <- getCanvas img
  putStrLn (printf "Show image %s" (show path))
  setImageFile path Nothing img
  err <- getImageLoadError img
  case err of
    EvasLoadErrorNone -> return ()
    _ -> putStrLn =<< peekCString =<< loadErrorString (fromEnum err)
  (w,h) <- getImageSize img
  resize w h img
  refresh img canvas

-- Zoom the image so that it fits in the canvas
zoomFit :: Object -> Canvas -> IO ()
zoomFit img canvas = do
  (cw,ch) <- getCanvasOutputSize canvas
  (iw,ih) <- getImageSize img
  
  let ratioH = (fromIntegral ch) / (fromIntegral ih)
      ratioW = (fromIntegral cw) / (fromIntegral iw)
      ratio = min 1.0 (min ratioH ratioW)
      w = floor $ (ratio * fromIntegral iw :: Double)
      h = floor $ (ratio * fromIntegral ih :: Double)

  resize w h img

-- Center the image on the canvas
centerImage :: Object -> Canvas -> IO ()
centerImage img canvas = do
  (cw,ch) <- getCanvasOutputSize canvas
  (_,_,iw,ih) <- getGeometry img
  let w = floor $ (fromIntegral cw - fromIntegral iw :: Double) / 2
      h = floor $ (fromIntegral ch - fromIntegral ih :: Double) / 2
  
  move w h img

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

onWindowResize :: Window -> IO () -> IO ()
onWindowResize win cb = setWindowResizeCallback win (const cb)

-- Configure background with "backgroundColor"
configureBackground :: Window -> Canvas -> IO Object
configureBackground win canvas = do
  bg <- addRectangle canvas
  let (red, green, blue, alpha) = backgroundColor
  setColor red green blue alpha bg
  (w,h) <- getCanvasOutputSize canvas
  resize w h bg
  uncover bg
  setFocus True bg

  return bg
