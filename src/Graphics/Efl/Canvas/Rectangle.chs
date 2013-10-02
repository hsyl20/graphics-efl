{-# Language ForeignFunctionInterface #-}

-- | Rectangle shape
module Graphics.Efl.Canvas.Rectangle where

import Graphics.Efl.Canvas.Types

-- | Add a rectangle to the canvas
foreign import ccall "evas_object_rectangle_add" addRectangle :: Canvas -> IO Object

