{-# Language MultiWayIf,LambdaCase #-}

import qualified Graphics.Efl.Core as Core
import Graphics.Efl.Window
import Graphics.Efl.Canvas

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad (void)
import Foreign.C.String
import Foreign.Ptr
import System.Environment (getArgs)
import System.Exit
import Text.Printf

import qualified Data.Vector as Vector
import Data.Vector ((!), Vector)


backgroundColor :: (Int,Int,Int,Int)
backgroundColor = (0,0,0,0)

main :: IO ()
main = do
  initWindowingSystem
  win <- createWindow Nothing 0 0 800 600 Nothing
  showWindow win

  canvas <- getWindowCanvas win

  bg <- configureBackground win canvas

  images <- Vector.fromList <$> getArgs
  currentImage <- newMVar 0

  putStrLn (printf "%d images to show" (Vector.length images))

  if Vector.length images == 0 then myShutdown win else return ()

  img <- addImage canvas

  showImage img $ images ! 0

  enablePassEvents img
  uncover img

  tr <- createMap 4
  populateMapPointsFromObject tr img
--  setMap img tr
--  enableMap img
  freeMap tr


  onCanvasResize win $ do
    zoomFit img canvas
    centerImage img canvas

  onKeyDown bg $ \case 
    "space" -> nextImage img currentImage images
    "n" -> nextImage img currentImage images
    "p" -> previousImage img currentImage images
    "t" -> rotate img 90.0
    "q" -> Core.quitMainLoop
    _ -> return ()

  onMouseDown bg $ do
    putStrLn "Mouse down"

  Core.beginMainLoop

  myShutdown win

-- Shutdown the application
myShutdown :: Window -> IO ()
myShutdown win = do
  putStrLn "Going to shutdown"
-- FIXME: deadlock
  destroyWindow win
  shutdownWindowingSystem
  exitSuccess

-- Refresh current display
refresh :: Object -> Canvas -> IO ()
refresh img canvas = do
  zoomFit img canvas
  centerImage img canvas


rotate :: Object -> Double -> IO ()
rotate img angle = do
   (x,y,w,h) <- getGeometry img
   tr <- dupMap =<< getMap img
   rotateMap tr angle (x + w `div` 2) (y + h `div` 2)
   setMap img tr
   enableMap img
   freeMap tr

-- Switch to next image
nextImage :: Object -> MVar Int -> Vector String -> IO ()
nextImage img current images = do
  c <- takeMVar current
  if c+1 < Vector.length images
    then do
      showImage img (images ! (c+1))
      putMVar current (c+1)
    else
      putMVar current c

-- Switch to previous image
previousImage :: Object -> MVar Int -> Vector String -> IO ()
previousImage img current images = do
  c <- takeMVar current
  if c > 0 then do
      showImage img (images ! (c-1))
      putMVar current (c-1)
    else 
      putMVar current c

-- Show the image whose path is given as a parameter
showImage :: Object -> String -> IO ()
showImage img path = do
  canvas <- getCanvas img
  putStrLn (printf "Show image %s" (show path))
  withCString path $ flip (setImageFile img) nullPtr
  err <- getImageLoadError img
  case err of
    EvasLoadErrorNone -> return ()
    _ -> putStrLn =<< peekCString =<< loadErrorString (fromEnum err)
  (w,h) <- getImageSize img
  setImageFill img 0 0 w h
  resize img w h
  refresh img canvas

-- Zoom the image so that it fits in the canvas
zoomFit :: Object -> Canvas -> IO ()
zoomFit img canvas = do
  (cw,ch) <- getOutputSize canvas
  (iw,ih) <- getImageSize img
  
  let ratioH = (fromIntegral ch) / (fromIntegral ih)
      ratioW = (fromIntegral cw) / (fromIntegral iw)
      ratio = min 1.0 (min ratioH ratioW)
      w = floor $ (ratio * fromIntegral iw :: Double)
      h = floor $ (ratio * fromIntegral ih :: Double)

  resize img w h
  setImageFill img 0 0 w h

-- Center the image on the canvas
centerImage :: Object -> Canvas -> IO ()
centerImage img canvas = do
  (cw,ch) <- getOutputSize canvas
  (_,_,iw,ih) <- getGeometry img
  let w = floor $ (fromIntegral cw - fromIntegral iw :: Double) / 2
      h = floor $ (fromIntegral ch - fromIntegral ih :: Double) / 2
  
  move img w h

onMouseDown :: Object -> IO () -> IO ()
onMouseDown = onEvent EvasCallbackMouseDown

onKeyDown :: Object -> (String -> IO ()) -> IO ()
onKeyDown obj cb = do 
  wcb <- wrapEventCallback $ \_ _ _ info -> do
    keyName <- keyDownKey info
    cb keyName
  void $ addObjectEventCallback obj EvasCallbackKeyDown wcb nullPtr
  
onEvent :: CallbackType -> Object -> IO () -> IO ()
onEvent evType obj cb = do
  wcb <- wrapEventCallback $ \_ _ _ _ -> cb
  void $ addObjectEventCallback obj evType wcb nullPtr

onCanvasResize :: Window -> IO () -> IO ()
onCanvasResize win cb = setWindowResizeCallback win (const cb)

-- Configure background with "backgroundColor"
configureBackground :: Window -> Canvas -> IO Object
configureBackground win canvas = do
  bg <- addRectangle canvas
  let (red, green, blue, alpha) = backgroundColor
  setColor bg red green blue alpha
  (w,h) <- getOutputSize canvas
  resize bg w h
  uncover bg
  setFocus bg True

  onCanvasResize win $ do
    (lw,lh) <- getOutputSize canvas
    resize bg lw lh

  return bg
