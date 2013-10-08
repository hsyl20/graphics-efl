{-# Language ForeignFunctionInterface #-}

-- | Line shape
module Graphics.Efl.Canvas.Line (
   addLine, setLineCoords, getLineCoords
) where

import Graphics.Efl.Canvas.Types
import Graphics.Efl.Helpers
import Foreign.C.Types
import Foreign.Ptr

-- | Add a line to the canvas
foreign import ccall "evas_object_line_add" addLine :: Canvas -> IO Object

-- | Set the coordinates of the end points of the given line object
setLineCoords :: Coord -> Coord -> Coord -> Coord -> Object -> IO ()
setLineCoords x1 y1 x2 y2 obj = _setLineCoords obj x1 y1 x2 y2

foreign import ccall "evas_object_line_xy_set" _setLineCoords :: Object -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Retrieve the coordinates of the end points of the given line object
getLineCoords :: Object -> IO (Coord, Coord, Coord, Coord)
getLineCoords obj = get4_helper (_getLineCoords obj)

foreign import ccall "evas_object_line_xy_get" _getLineCoords :: Object -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()
