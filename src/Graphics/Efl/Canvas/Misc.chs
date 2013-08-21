{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Misc where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Graphics.Efl.Helpers
import Graphics.Efl.Canvas.Types

#include <Evas.h>

{- FIXME: put this somewhere else... -}

foreign import ccall "wrapper" wrapEventCallback :: (Ptr () -> Canvas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb

getOutputSize :: Canvas -> IO (Int,Int)
getOutputSize ev = get2_helper (evas_output_size_get_ ev)

foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Canvas -> Ptr Int -> Ptr Int -> IO ()


foreign import ccall "evas_event_callback_add" addEventCallback :: Canvas -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" loadErrorString :: Int -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
