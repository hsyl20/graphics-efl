{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Misc where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Graphics.Efl.Canvas.Types

#include <Evas.h>

{- FIXME: put this somewhere else... -}

foreign import ccall "wrapper" wrapEventCallback :: (Ptr () -> Canvas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb

foreign import ccall "evas_event_callback_add" addCanvasEventCallback :: Canvas -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" loadErrorString :: Int -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
