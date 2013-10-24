{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Misc (
   loadErrorString
) where

import Foreign.C.String
import Foreign.C.Types

#include <Evas.h>

-- | Return error string
loadErrorString :: Int -> IO String
loadErrorString n = peekCString =<< _loadErrorString (fromIntegral n)

foreign import ccall "evas_load_error_str" _loadErrorString :: CInt -> IO CString
