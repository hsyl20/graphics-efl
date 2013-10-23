{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Misc (
   loadErrorString,
   keyDownKeyname, keyDownKey
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

#include <Evas.h>

{- FIXME: put this somewhere else... -}

-- | Return error string
loadErrorString :: Int -> IO String
loadErrorString n = peekCString =<< _loadErrorString (fromIntegral n)

foreign import ccall "evas_load_error_str" _loadErrorString :: CInt -> IO CString


keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
