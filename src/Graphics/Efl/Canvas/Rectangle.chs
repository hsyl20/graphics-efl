{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Rectangle where

import Graphics.Efl.Canvas.Types

-- | Add a rectangle to the given evas
foreign import ccall "evas_object_rectangle_add" evas_object_rectangle_add :: Canvas -> IO Object

