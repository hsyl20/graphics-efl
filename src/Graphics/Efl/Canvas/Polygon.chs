{-# Language ForeignFunctionInterface #-}

-- | Polygon shape
module Graphics.Efl.Canvas.Polygon (
   addPolygon, addPolygonPoint, addPolygonPoints,
   clearPolygonPoints
) where

import Graphics.Efl.Canvas.Types
import Foreign.C.Types

-- | Add a polygon to the canvas
foreign import ccall "evas_object_polygon_add" addPolygon :: Canvas -> IO Object


-- | Add the given point to the polygon
addPolygonPoint :: Coord -> Coord -> Object -> IO ()
addPolygonPoint x y obj = _addPolygonPoint obj x y

foreign import ccall "evas_object_polygon_point_add" _addPolygonPoint :: Object -> Coord -> Coord -> IO ()

-- | Add the given points to the polygon
addPolygonPoints :: [(Coord,Coord)] -> Object -> IO ()
addPolygonPoints xys obj = mapM_ (uncurry (_addPolygonPoint obj)) xys

-- | Remove all the points of the polygon
foreign import ccall "evas_object_polygon_points_clear" clearPolygonPoints :: Object -> IO ()
