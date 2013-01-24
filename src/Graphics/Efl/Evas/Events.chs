{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Evas.Events where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Graphics.Efl.Helpers
import Graphics.Efl.Evas.Types

#include <Evas.h>

foreign import ccall "evas_object_pass_events_set" evas_object_pass_events_set :: Object -> Bool -> IO ()

foreign import ccall "evas_object_image_load_error_get" evas_object_image_load_error_get :: Object -> IO Int
foreign import ccall "evas_object_image_fill_set" evas_object_image_fill_set :: Object -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall "evas_object_event_callback_add" evas_object_event_callback_add :: Object -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "wrapper" evas_object_event_wrap_callback :: (Ptr () -> Evas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb



type ObjectEventCb = FunPtr (Ptr () -> Evas -> Object -> Ptr () -> IO ())


foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Evas -> Ptr Int -> Ptr Int -> IO ()

evas_output_size_get :: Evas -> IO (Int,Int)
evas_output_size_get ev = get2_helper (evas_output_size_get_ ev)


foreign import ccall "evas_event_callback_add" evas_event_callback_add :: Evas -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" evas_load_error_str :: Int -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
