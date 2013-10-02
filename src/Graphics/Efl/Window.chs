{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Window (
   Window,
   initWindowingSystem, shutdownWindowingSystem,
   createWindow, destroyWindow, showWindow,
   getWindowCanvas,
   setWindowResizeCallback
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.Maybe
import Control.Applicative
import Control.Monad

import Graphics.Efl.Canvas

#include <Ecore_Evas.h>

type Window = Ptr ()

-- | Initialize the windowing system
initWindowingSystem :: IO ()
initWindowingSystem = do
   err <- initWindowingSystem_
   case err of
      0 -> error "Error during initialization"
      _ -> return ()

foreign import ccall "ecore_evas_init" initWindowingSystem_ :: IO Int

-- | Shut down the windowing system
foreign import ccall "ecore_evas_shutdown" shutdownWindowingSystem :: IO ()

-- | Create a new window
createWindow :: Maybe String -> Int -> Int -> Int -> Int -> Maybe String -> IO Window
createWindow engine x y w h options = do
   let wrap s = fromMaybe (return nullPtr) (newCAString <$> s)
   engine' <- wrap engine
   options' <- wrap options
   win <- createWindow_ engine' x y w h options'
   when (engine' /= nullPtr) (free engine')
   when (options' /= nullPtr) (free options')
   return win

foreign import ccall "ecore_evas_new" createWindow_ :: CString -> Int -> Int -> Int -> Int -> CString -> IO Window

-- | Destroy a window
foreign import ccall "ecore_evas_free" destroyWindow :: Window -> IO ()

-- | Display a window
foreign import ccall "ecore_evas_show" showWindow :: Window -> IO ()

-- | Get the rendering canvas of the window
foreign import ccall "ecore_evas_get" getWindowCanvas :: Window -> IO Canvas

-- | Associate a callback to the "resize" event
setWindowResizeCallback :: Window -> (Window -> IO ()) -> IO ()
setWindowResizeCallback win cb = setWindowResizeCallback_ win =<< wrapCallback cb

foreign import ccall "ecore_evas_callback_resize_set" setWindowResizeCallback_ :: Window -> FunPtr (Window -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapCallback :: (Window -> IO ()) -> IO (FunPtr (Window -> IO ()))
