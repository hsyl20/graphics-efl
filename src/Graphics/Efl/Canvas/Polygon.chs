{-# Language ForeignFunctionInterface #-}

-- | Polygon shape
module Graphics.Efl.Canvas.Polygon where

import Graphics.Efl.Canvas.Types

-- | Add a polygon to the canvas
foreign import ccall "evas_object_polygon_add" addPolygon :: Canvas -> IO Object


-- | Add the given point to the polygon
foreign import ccall "evas_object_polygon_point_add" addPolygonPoint :: Object -> Coord -> Coord -> IO ()

-- | Remove all the points of the polygon
foreign import ccall "evas_object_polygon_points_clear" clearPolygonPoints :: Object -> IO ()
