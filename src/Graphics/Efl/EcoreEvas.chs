{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.EcoreEvas where

import Foreign.Ptr
import Foreign.C.String

import Graphics.Efl.Evas

#include <Ecore_Evas.h>

type EcoreEvas = Ptr ()

foreign import ccall "ecore_evas_init" ecore_evas_init :: IO ()
foreign import ccall "ecore_evas_shutdown" ecore_evas_shutdown :: IO ()

foreign import ccall "ecore_evas_new" ecore_evas_new :: CString -> Int -> Int -> Int -> Int -> CString -> IO EcoreEvas
foreign import ccall "ecore_evas_free" ecore_evas_free :: EcoreEvas -> IO ()
foreign import ccall "ecore_evas_show" ecore_evas_show :: EcoreEvas -> IO ()

foreign import ccall "ecore_evas_get" ecore_evas_get :: EcoreEvas -> IO Evas

foreign import ccall "ecore_evas_callback_resize_set" ecore_evas_callback_resize_set :: EcoreEvas -> FunPtr (EcoreEvas -> IO ()) -> IO ()

foreign import ccall "wrapper" ecore_evas_wrap_callback :: (EcoreEvas -> IO ()) -> IO (FunPtr (EcoreEvas -> IO ()))
