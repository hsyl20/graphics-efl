{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Misc (
   loadErrorString,
   wrapEventCallback, keyDownKeyname, keyDownKey
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Graphics.Efl.Canvas.Types

#include <Evas.h>

{- FIXME: put this somewhere else... -}

foreign import ccall "wrapper" wrapEventCallback :: (Ptr () -> Canvas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb

foreign import ccall "evas_event_callback_add" addCanvasEventCallback :: Canvas -> CInt -> ObjectEventCb -> Ptr () -> IO ()

-- | Return error string
loadErrorString :: Int -> IO String
loadErrorString n = peekCString =<< _loadErrorString (fromIntegral n)

foreign import ccall "evas_load_error_str" _loadErrorString :: CInt -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
