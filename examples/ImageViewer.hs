{-# Language MultiWayIf,LambdaCase #-}

import qualified Graphics.Efl.Core as Core
import Graphics.Efl.CoreCanvas (CoreCanvas)
import qualified Graphics.Efl.CoreCanvas as CoreCanvas
import Graphics.Efl.Canvas (Canvas,Object,LoadError(..))
import qualified Graphics.Efl.Canvas as Canvas
import qualified Graphics.Efl.Canvas.Transformations as Trans

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
  CoreCanvas.init
  ee <- CoreCanvas.new nullPtr 0 0 800 600 nullPtr
  CoreCanvas.show ee

  canvas <- CoreCanvas.get ee

  bg <- configureBackground ee canvas

  images <- Vector.fromList <$> getArgs
  currentImage <- newMVar 0

  putStrLn (printf "%d images to show" (Vector.length images))

  if Vector.length images == 0 then myShutdown ee else return ()

  img <- Canvas.addImage canvas

  showImage img $ images ! 0

  Canvas.enablePassEvents img
  Canvas.show img

  tr <- Trans.new 4
  Trans.populateFromObject tr img
--  Canvas.setTransformation img tr
--  Canvas.enableTransformation img
  Trans.free tr


  onCanvasResize ee $ do
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

  myShutdown ee

-- Shutdown the application
myShutdown :: CoreCanvas -> IO ()
myShutdown ee = do
  putStrLn "Going to shutdown"
-- FIXME: deadlock
  CoreCanvas.free ee
  CoreCanvas.shutdown
  exitSuccess

-- Refresh current display
refresh :: Object -> Canvas -> IO ()
refresh img canvas = do
  zoomFit img canvas
  centerImage img canvas


rotate :: Object -> Double -> IO ()
rotate img angle = do
   (x,y,w,h) <- Canvas.getGeometry img
   tr <- Trans.duplicate =<< Canvas.getTransformation img
   Trans.rotate tr angle (x + w `div` 2) (y + h `div` 2)
   Canvas.setTransformation img tr
   Canvas.enableTransformation img
   Trans.free tr

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
  canvas <- Canvas.getCanvas img
  putStrLn (printf "Show image %s" (show path))
  withCString path $ flip (Canvas.setImageFile img) nullPtr
  err <- Canvas.getImageLoadError img
  case err of
    EvasLoadErrorNone -> return ()
    _ -> putStrLn =<< peekCString =<< Canvas.loadErrorString (fromEnum err)
  (w,h) <- Canvas.getImageSize img
  Canvas.setImageFill img 0 0 w h
  Canvas.resize img w h
  refresh img canvas

-- Zoom the image so that it fits in the canvas
zoomFit :: Object -> Canvas -> IO ()
zoomFit img canvas = do
  (cw,ch) <- Canvas.getOutputSize canvas
  (iw,ih) <- Canvas.getImageSize img
  
  let ratioH = (fromIntegral ch) / (fromIntegral ih)
      ratioW = (fromIntegral cw) / (fromIntegral iw)
      ratio = min 1.0 (min ratioH ratioW)
      w = floor $ (ratio * fromIntegral iw :: Double)
      h = floor $ (ratio * fromIntegral ih :: Double)

  Canvas.resize img w h
  Canvas.setImageFill img 0 0 w h

-- Center the image on the canvas
centerImage :: Object -> Canvas -> IO ()
centerImage img canvas = do
  (cw,ch) <- Canvas.getOutputSize canvas
  (_,_,iw,ih) <- Canvas.getGeometry img
  let w = floor $ (fromIntegral cw - fromIntegral iw :: Double) / 2
      h = floor $ (fromIntegral ch - fromIntegral ih :: Double) / 2
  
  Canvas.move img w h

onMouseDown :: Object -> IO () -> IO ()
onMouseDown = onEvent Canvas.EvasCallbackMouseDown

onKeyDown :: Object -> (String -> IO ()) -> IO ()
onKeyDown obj cb = do 
  wcb <- Canvas.wrapEventCallback $ \_ _ _ info -> do
    keyName <- Canvas.keyDownKey info
    cb keyName
  void $ Canvas.addObjectEventCallback obj Canvas.EvasCallbackKeyDown wcb nullPtr
  
onEvent :: Canvas.CallbackType -> Object -> IO () -> IO ()
onEvent evType obj cb = do
  wcb <- Canvas.wrapEventCallback $ \_ _ _ _ -> cb
  void $ Canvas.addObjectEventCallback obj evType wcb nullPtr

onCanvasResize :: CoreCanvas -> IO () -> IO ()
onCanvasResize ee cb = do
  wcb <- CoreCanvas.wrapCallback (\_ -> cb)
  CoreCanvas.setResizeCallback ee wcb

-- Configure background with "backgroundColor"
configureBackground :: CoreCanvas -> Canvas -> IO Object
configureBackground ee canvas = do
  bg <- Canvas.addRectangle canvas
  let (alpha, red, green, blue) = backgroundColor
  Canvas.setColor bg alpha red green blue
  (w,h) <- Canvas.getOutputSize canvas
  Canvas.resize bg w h
  Canvas.show bg
  Canvas.setFocus bg True

  onCanvasResize ee $ do
    (lw,lh) <- Canvas.getOutputSize canvas
    Canvas.resize bg lw lh

  return bg
