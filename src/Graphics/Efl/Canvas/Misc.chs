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

foreign import ccall "wrapper" evas_object_event_wrap_callback :: (Ptr () -> Canvas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb

foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Canvas -> Ptr Int -> Ptr Int -> IO ()

evas_output_size_get :: Canvas -> IO (Int,Int)
evas_output_size_get ev = get2_helper (evas_output_size_get_ ev)


foreign import ccall "evas_event_callback_add" evas_event_callback_add :: Canvas -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" evas_load_error_str :: Int -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
