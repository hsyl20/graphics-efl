{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.CoreCanvas where

import Foreign.Ptr
import Foreign.C.String

import Graphics.Efl.Canvas

#include <Ecore_Evas.h>

type CoreCanvas = Ptr ()

foreign import ccall "ecore_evas_init" init :: IO ()
foreign import ccall "ecore_evas_shutdown" shutdown :: IO ()

foreign import ccall "ecore_evas_new" new :: CString -> Int -> Int -> Int -> Int -> CString -> IO CoreCanvas
foreign import ccall "ecore_evas_free" free :: CoreCanvas -> IO ()
foreign import ccall "ecore_evas_show" show :: CoreCanvas -> IO ()

foreign import ccall "ecore_evas_get" get :: CoreCanvas -> IO Canvas

foreign import ccall "ecore_evas_callback_resize_set" setResizeCallback :: CoreCanvas -> FunPtr (CoreCanvas -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapCallback :: (CoreCanvas -> IO ()) -> IO (FunPtr (CoreCanvas -> IO ()))
