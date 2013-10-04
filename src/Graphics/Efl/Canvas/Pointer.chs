{-# Language ForeignFunctionInterface #-}

-- | Pointer and coordinates methods
module Graphics.Efl.Canvas.Pointer (
   convertWorldToScreen, convertScreenToWorld,
   convertWorldToScreenX, convertWorldToScreenY,
   convertScreenToWorldX, convertScreenToWorldY,
   getScreenPointer, getCanvasPointer,
   isPointerInside, getPointerButtons
) where


import Graphics.Efl.Canvas.Types
import Graphics.Efl.Eina
import Graphics.Efl.Helpers

import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative

-- | Convert screen coordinates to canvas coordinates
convertScreenToWorld :: Canvas -> Int -> Int -> IO (Coord,Coord)
convertScreenToWorld c x y = do
   x' <- convertScreenToWorldX c x
   y' <- convertScreenToWorldY c y
   return (x',y')

-- | Convert canvas coordinates to screen coordinates
convertWorldToScreen :: Canvas -> Coord -> Coord -> IO (Int,Int)
convertWorldToScreen c x y = do
   x' <- convertWorldToScreenX c x
   y' <- convertWorldToScreenY c y
   return (x',y')

-- | Convert/scale an output screen coordinate into canvas coordinate
foreign import ccall "evas_coord_screen_x_to_world" convertScreenToWorldX :: Canvas -> Int -> IO Coord

-- | Convert/scale an output screen coordinate into canvas coordinate
foreign import ccall "evas_coord_screen_y_to_world" convertScreenToWorldY :: Canvas -> Int -> IO Coord

-- | Convert/scale an output canvas coordinate into screen coordinate
foreign import ccall "evas_coord_world_x_to_screen" convertWorldToScreenX :: Canvas -> Coord -> IO Int

-- | Convert/scale an output canvas coordinate into screen coordinate
foreign import ccall "evas_coord_world_y_to_screen" convertWorldToScreenY :: Canvas -> Coord -> IO Int



-- | Return the current known pointer coordinates
getScreenPointer :: Canvas -> IO (Int,Int)
getScreenPointer canvas = get2_helper(_getScreenPointer canvas)

foreign import ccall "evas_pointer_output_xy_get" _getScreenPointer :: Canvas -> Ptr Int -> Ptr Int -> IO ()

-- | Return the current known pointer coordinates
getCanvasPointer :: Canvas -> IO (Coord,Coord)
getCanvasPointer canvas = get2_helper(_getCanvasPointer canvas)

foreign import ccall "evas_pointer_canvas_xy_get" _getCanvasPointer :: Canvas -> Ptr Coord -> Ptr Coord -> IO ()


-- | Return a bitmask with the mouse buttons currently pressed, set to 1
foreign import ccall "evas_pointer_button_down_mask_get" getPointerButtons :: Canvas -> IO Int

-- | Return whether the mouse pointer is logically inside the canvas
isPointerInside :: Canvas -> IO Bool
isPointerInside canvas = toBool <$> _isPointerInside canvas

foreign import ccall "evas_pointer_inside_get" _isPointerInside :: Canvas -> IO EinaBool
