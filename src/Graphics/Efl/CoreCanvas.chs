{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.CoreCanvas where

import Foreign.Ptr
import Foreign.C.String

import Graphics.Efl.Canvas

#include <Ecore_Evas.h>

type CoreCanvas = Ptr ()

foreign import ccall "ecore_evas_init" ecore_evas_init :: IO ()
foreign import ccall "ecore_evas_shutdown" ecore_evas_shutdown :: IO ()

foreign import ccall "ecore_evas_new" ecore_evas_new :: CString -> Int -> Int -> Int -> Int -> CString -> IO CoreCanvas
foreign import ccall "ecore_evas_free" ecore_evas_free :: CoreCanvas -> IO ()
foreign import ccall "ecore_evas_show" ecore_evas_show :: CoreCanvas -> IO ()

foreign import ccall "ecore_evas_get" ecore_evas_get :: CoreCanvas -> IO Canvas

foreign import ccall "ecore_evas_callback_resize_set" ecore_evas_callback_resize_set :: CoreCanvas -> FunPtr (CoreCanvas -> IO ()) -> IO ()

foreign import ccall "wrapper" ecore_evas_wrap_callback :: (CoreCanvas -> IO ()) -> IO (FunPtr (CoreCanvas -> IO ()))
