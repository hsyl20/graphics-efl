{-# Language MultiWayIf,LambdaCase #-}

import System.Environment (getArgs)
import System.Exit

import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Applicative ((<$>))

import Foreign.Ptr
import Foreign.C.String


import Graphics.Efl.Core
import Graphics.Efl.CoreCanvas
import Graphics.Efl.Canvas

import qualified Data.Vector as Vector
import Data.Vector ((!), Vector)

backgroundColor :: (Int,Int,Int,Int)
backgroundColor = (0,0,0,0)

main :: IO ()
main = do
  ecore_evas_init
  ee <- ecore_evas_new nullPtr 0 0 800 600 nullPtr
  ecore_evas_show ee

  canvas <- ecore_evas_get ee

  bg <- configureBackground ee canvas

  images <- Vector.fromList <$> getArgs
  currentImage <- newMVar 0

  if Vector.length images == 0 then shutdown ee else return ()

  img <- object_image_add canvas
  object_pass_events_set img True
  object_show img

  showImage img $ images ! 0

  onCanvasResize ee $ do
    zoomFit img canvas
    centerImage img canvas

  onKeyDown bg $ \case 
    "space" -> nextImage img currentImage images >> refresh img canvas
    "n" -> nextImage img currentImage images >> refresh img canvas
    "p" -> previousImage img currentImage images >> refresh img canvas
    "q" -> ecore_main_loop_quit
    _ -> return ()

  onMouseDown bg $ do
    putStrLn "Mouse down"

  ecore_main_loop_begin

  shutdown ee

-- Shutdown the application
shutdown :: CoreCanvas -> IO ()
shutdown ee = do
  putStrLn "Going to shutdown"
-- FIXME: deadlock
--  ecore_evas_free ee
--  ecore_evas_shutdown
  exitSuccess

-- Refresh current display
refresh :: Object -> Canvas -> IO ()
refresh img canvas = do
  zoomFit img canvas
  centerImage img canvas

-- Switch to next image
nextImage :: Object -> MVar Int -> Vector String -> IO ()
nextImage img current images = do
  c <- takeMVar current
  if Vector.length images >= c
    then do
      showImage img (images ! (c+1))
      putMVar current (c+1)
    else
      ecore_main_loop_quit

-- Switch to previous image
previousImage :: Object -> MVar Int -> Vector String -> IO ()
previousImage img current images = do
  c <- takeMVar current
  if c > 0 then do
      showImage img (images ! (c-1))
      putMVar current (c-1)
    else 
      return ()

-- Show the image whose path is given as a parameter
showImage :: Object -> String -> IO ()
showImage img path = do
  putStrLn $ "Show image " ++ path
  withCString path $ flip (object_image_file_set img) nullPtr
  err <- object_image_load_error_get img
  case err of
    EvasLoadErrorNone -> return ()
    _ -> putStrLn =<< peekCString =<< evas_load_error_str (fromEnum err)
  (w,h) <- object_image_size_get img
  object_image_fill_set img 0 0 w h
  object_resize img w h


-- Zoom the image so that it fits in the canvas
zoomFit :: Object -> Canvas -> IO ()
zoomFit img canvas = do
  (cw,ch) <- evas_output_size_get canvas
  (iw,ih) <- object_image_size_get img
  
  let ratioH = (fromIntegral ch) / (fromIntegral ih)
      ratioW = (fromIntegral cw) / (fromIntegral iw)
      ratio = min 1.0 (min ratioH ratioW)
      w = floor $ (ratio * fromIntegral iw :: Double)
      h = floor $ (ratio * fromIntegral ih :: Double)

  object_resize img w h
  object_image_fill_set img 0 0 w h

-- Center the image on the canvas
centerImage :: Object -> Canvas -> IO ()
centerImage img canvas = do
  (cw,ch) <- evas_output_size_get canvas
  (_,_,iw,ih) <- object_geometry_get img
  let w = floor $ (fromIntegral cw - fromIntegral iw :: Double) / 2
      h = floor $ (fromIntegral ch - fromIntegral ih :: Double) / 2
  
  object_move img w h

onMouseDown :: Object -> IO () -> IO ()
onMouseDown = onEvent EvasCallbackMouseDown

onKeyDown :: Object -> (String -> IO ()) -> IO ()
onKeyDown obj cb = do 
  wcb <- evas_object_event_wrap_callback $ \_ _ _ info -> do
    keyName <- keyDownKey info
    cb keyName
  void $ object_event_callback_add obj EvasCallbackKeyDown wcb nullPtr
  
onEvent :: CallbackType -> Object -> IO () -> IO ()
onEvent evType obj cb = do
  wcb <- evas_object_event_wrap_callback $ \_ _ _ _ -> cb
  void $ object_event_callback_add obj evType wcb nullPtr

onCanvasResize :: CoreCanvas -> IO () -> IO ()
onCanvasResize ee cb = do
  wcb <- ecore_evas_wrap_callback (\_ -> cb)
  ecore_evas_callback_resize_set ee wcb

-- Configure background with "backgroundColor"
configureBackground :: CoreCanvas -> Canvas -> IO Object
configureBackground ee canvas = do
  bg <- evas_object_rectangle_add canvas
  let (alpha, red, green, blue) = backgroundColor
  object_color_set bg alpha red green blue
  (w,h) <- evas_output_size_get canvas
  object_resize bg w h
  object_show bg
  object_focus_set bg True

  onCanvasResize ee $ do
    (lw,lh) <- evas_output_size_get canvas
    object_resize bg lw lh

  return bg
