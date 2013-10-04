{-# Language ForeignFunctionInterface #-}

-- | Rendering engine
module Graphics.Efl.Canvas.ViewPort (
   setCanvasOutputSize, getCanvasOutputSize,
   setCanvasViewport, getCanvasViewport,
   setCanvasFramespace, getCanvasFramespace
) where

import Foreign.Ptr

import Graphics.Efl.Canvas.Types
import Graphics.Efl.Helpers

-- | Set the output size of the render engine of the given canvas
foreign import ccall "evas_output_size_set" setCanvasOutputSize :: Canvas -> Int -> Int -> IO ()

-- | Retrieve the output size of the render engine of the given evas.
getCanvasOutputSize :: Canvas -> IO (Int,Int)
getCanvasOutputSize canvas = get2_helper (_getCanvasOutputSize canvas)

foreign import ccall "evas_output_size_get" _getCanvasOutputSize :: Canvas -> Ptr Int -> Ptr Int -> IO ()


-- | Set the output viewport of the given evas in canvas units.
foreign import ccall "evas_output_viewport_set" setCanvasViewport :: Canvas -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the render engine's output viewport co-ordinates in canvas units
getCanvasViewport :: Canvas -> IO (Coord,Coord,Coord,Coord)
getCanvasViewport canvas = get4_helper (_getCanvasViewport canvas)

foreign import ccall "evas_output_viewport_get" _getCanvasViewport :: Canvas -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()


-- | Set the output framespace size of the render engine of the given canvas
foreign import ccall "evas_output_framespace_set" setCanvasFramespace :: Canvas -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the render engine's output framespace co-ordinates in canvas units
getCanvasFramespace :: Canvas -> IO (Coord,Coord,Coord,Coord)
getCanvasFramespace canvas = get4_helper (_getCanvasFramespace canvas)

foreign import ccall "evas_output_framespace_get" _getCanvasFramespace :: Canvas -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()
