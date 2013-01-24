{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Evas.Rectangle where

import Graphics.Efl.Evas.Types

-- | Add a rectangle to the given evas
foreign import ccall "evas_object_rectangle_add" evas_object_rectangle_add :: Evas -> IO Object

