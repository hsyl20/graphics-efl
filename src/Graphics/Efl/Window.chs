{-# Language ForeignFunctionInterface #-}

-- | EFL windowing system
module Graphics.Efl.Window (
   Window,
   initWindowingSystem, shutdownWindowingSystem,
   isEngineSupported, getEngines, getEngineName,
   createWindow, destroyWindow,
   showWindow, hideWindow, setWindowVisible, getWindowVisible,
   onWindowHide, onWindowHideEx,
   onWindowShow, onWindowShowEx,
   setWindowTitle, getWindowTitle,
   getWindowCanvas, getWindowGeometry, setWindowGeometry,
   getWindowSize, getWindowPosition,
   resizeWindow, onWindowResize, onWindowResizeEx,
   moveWindow, onWindowMove, onWindowMoveEx,
   getWindowFocus, setWindowFocus, onWindowFocusIn, onWindowFocusOut
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Data.Maybe
import Control.Applicative
import Control.Monad

import Graphics.Efl.Canvas
import Graphics.Efl.Helpers
import Graphics.Efl.Window.Types
import Graphics.Efl.Eina

#include <Ecore_Evas.h>

-- | A window
type Window = Ptr ()

-- | Initialize the windowing system
initWindowingSystem :: IO ()
initWindowingSystem = do
   err <- initWindowingSystem_
   case err of
      0 -> error "Error during initialization"
      _ -> return ()

foreign import ccall "ecore_evas_init" initWindowingSystem_ :: IO CInt

-- | Shut down the windowing system
foreign import ccall "ecore_evas_shutdown" shutdownWindowingSystem :: IO ()

-- | Indicate if an engine is supported
isEngineSupported :: EngineType -> IO Bool
isEngineSupported engine = (> 0) <$> _isEngineSupported (fromIntegral $ fromEnum engine)

foreign import ccall "ecore_evas_engine_type_supported_get" _isEngineSupported :: CInt -> IO CInt

-- | Return a list of supported engines names
getEngines :: IO [String]
getEngines = do
   xs <- _getEngines
   r <- mapM peekCString =<< toList xs
   _freeEngines xs
   return r

foreign import ccall "ecore_evas_engines_get" _getEngines :: IO (EinaList CString)
foreign import ccall "ecore_evas_engines_free" _freeEngines :: EinaList CString -> IO ()

-- | Get the name of the engine used by the window
getEngineName :: Window -> IO String
getEngineName win = peekCString =<< _getEngineName win

foreign import ccall "ecore_evas_engine_name_get" _getEngineName :: Window -> IO CString

-- | Create a new window
createWindow :: Maybe String -> Int -> Int -> Int -> Int -> Maybe String -> IO Window
createWindow engine x y w h options = do
   let wrap s = fromMaybe (return nullPtr) (newCAString <$> s)
   engine' <- wrap engine
   options' <- wrap options
   win <- createWindow_ engine'(fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) options'
   when (engine' /= nullPtr) (free engine')
   when (options' /= nullPtr) (free options')
   if win /= nullPtr 
      then return win
      else do
         let eng = fromMaybe "Default" (fmap show engine)
         putStrLn $ "Unable to create window with given engine (" ++ eng ++"). Falling back to default engine"
         createWindow Nothing x y w h options

foreign import ccall "ecore_evas_new" createWindow_ :: CString -> CInt -> CInt -> CInt -> CInt -> CString -> IO Window

-- | Destroy a window
foreign import ccall "ecore_evas_free" destroyWindow :: Window -> IO ()


-- | Set window title
setWindowTitle :: String -> Window -> IO ()
setWindowTitle title win = withCString title (_setWindowTitle win)

foreign import ccall "ecore_evas_title_set" _setWindowTitle :: Window -> CString -> IO ()

-- | Get window title
getWindowTitle :: Window -> IO String
getWindowTitle win = peekCString =<< _getWindowTitle win

foreign import ccall "ecore_evas_title_get" _getWindowTitle :: Window -> IO CString

-- | Show a window
foreign import ccall "ecore_evas_show" showWindow :: Window -> IO ()

-- | Hide a window
foreign import ccall "ecore_evas_hide" hideWindow :: Window -> IO ()

-- | Show/hide a window
setWindowVisible :: Bool -> Window -> IO ()
setWindowVisible True = showWindow
setWindowVisible False = hideWindow

-- | Is the window visible?
getWindowVisible :: Window -> IO Bool
getWindowVisible win = (==1) <$> getWindowVisible_ win

foreign import ccall "ecore_evas_visibility_get" getWindowVisible_ :: Window -> IO CInt

-- | Get the rendering canvas of the window
foreign import ccall "ecore_evas_get" getWindowCanvas :: Window -> IO Canvas

-- | Get window size
getWindowSize :: Window -> IO (Int,Int)
getWindowSize win = do
   (_,_,w,h) <- getWindowGeometry win
   return (w,h)

-- | Get window position
getWindowPosition :: Window -> IO (Int,Int)
getWindowPosition win = do
   (x,y,_,_) <- getWindowGeometry win
   return (x,y)

-- | Retrieve the position and (rectangular) size of the given canvas object
getWindowGeometry :: Window -> IO (Int,Int,Int,Int)
getWindowGeometry win = do
   (a,b,c,d) <- get4_helper (_getWindowGeometry win)
   return (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

foreign import ccall "ecore_evas_geometry_get" _getWindowGeometry :: Window -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | Set the position and (rectangular) size of the given canvas object
setWindowGeometry :: (Int,Int,Int,Int) -> Window -> IO ()
setWindowGeometry (x,y,w,h) win = _setWindowGeometry win (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

foreign import ccall "ecore_evas_move_resize" _setWindowGeometry :: Window ->  CInt ->  CInt ->  CInt ->  CInt -> IO ()

-- | Resize window
resizeWindow :: (Int,Int) -> Window -> IO ()
resizeWindow (w,h) win = _resizeWindow win (fromIntegral w) (fromIntegral h)

foreign import ccall "ecore_evas_resize" _resizeWindow :: Window ->  CInt ->  CInt ->  IO ()

-- | Move window
moveWindow :: (Int,Int) -> Window -> IO ()
moveWindow (x,y) win = _moveWindow win (fromIntegral x) (fromIntegral y)

foreign import ccall "ecore_evas_move" _moveWindow :: Window ->  CInt ->  CInt ->  IO ()

-- | Associate a callback to the "resize" event
onWindowResize :: Window -> IO () -> IO ()
onWindowResize win cb = onWindowResizeEx win (const cb)

-- | Associate a callback to the "resize" event
onWindowResizeEx :: Window -> (Window -> IO ()) -> IO ()
onWindowResizeEx win cb = setWindowResizeCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_resize_set" setWindowResizeCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

-- | Associate a callback to the "move" event
onWindowMove :: Window -> IO () -> IO ()
onWindowMove win cb = onWindowMoveEx win (const cb)

-- | Associate a callback to the "move" event
onWindowMoveEx :: Window -> (Window -> IO ()) -> IO ()
onWindowMoveEx win cb = setWindowMoveCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_move_set" setWindowMoveCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

-- | Associate a callback to the "hide" event
onWindowHide :: Window -> IO () -> IO ()
onWindowHide win cb = onWindowHideEx win (const cb)

-- | Associate a callback to the "hide" event
onWindowHideEx :: Window -> (Window -> IO ()) -> IO ()
onWindowHideEx win cb = setWindowHideCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_hide_set" setWindowHideCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

-- | Associate a callback to the "show" event
onWindowShow :: Window -> IO () -> IO ()
onWindowShow win cb = onWindowShowEx win (const cb)

-- | Associate a callback to the "show" event
onWindowShowEx :: Window -> (Window -> IO ()) -> IO ()
onWindowShowEx win cb = setWindowShowCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_show_set" setWindowShowCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

-- | Associate a callback to the "focus_in" event
onWindowFocusIn :: Window -> IO () -> IO ()
onWindowFocusIn win cb = onWindowFocusInEx win (const cb)

-- | Associate a callback to the "focus_in" event
onWindowFocusInEx :: Window -> (Window -> IO ()) -> IO ()
onWindowFocusInEx win cb = setWindowFocusInCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_focus_in_set" setWindowFocusInCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

-- | Associate a callback to the "focus_out" event
onWindowFocusOut :: Window -> IO () -> IO ()
onWindowFocusOut win cb = onWindowFocusOutEx win (const cb)

-- | Associate a callback to the "focus_out" event
onWindowFocusOutEx :: Window -> (Window -> IO ()) -> IO ()
onWindowFocusOutEx win cb = setWindowFocusOutCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_focus_out_set" setWindowFocusOutCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapCallback :: (Window -> IO ()) -> IO (FunPtr (Window -> IO ()))


-- | Set window focus
setWindowFocus :: Bool -> Window -> IO ()
setWindowFocus b w = _setWindowFocus w (fromBool b)

foreign import ccall "ecore_evas_focus_set" _setWindowFocus :: Window -> EinaBool ->  IO ()

-- | Get window focus
getWindowFocus :: Window -> IO Bool
getWindowFocus w = toBool <$> _getWindowFocus w

foreign import ccall "ecore_evas_focus_get" _getWindowFocus :: Window -> IO EinaBool
