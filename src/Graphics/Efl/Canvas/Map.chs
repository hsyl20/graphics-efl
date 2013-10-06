{-# Language ForeignFunctionInterface #-}

-- | Transformations applicable to canvas elements
module Graphics.Efl.Canvas.Map (
   enableMap, disableMap, isMapEnabled,
   setMap, getMap,
   populateMapPointsFromObjectFull, populateMapPointsFromObject, populateMapPointsFromGeometry,
   setMapPointsColor, rotateMap, zoomMap, rotateMap3D, lighting3DMap, perspective3DMap,
   isMapClockwise, createMap,
   setMapSmooth, isMapSmooth,
   setMapAlpha, isMapAlpha,
   dupMap, freeMap, getMapCount,
   setMapPointCoord, getMapPointCoord,
   setMapPointImageUV, getMapPointImageUV,
   setMapPointColor, getMapPointColor
) where

import Foreign.Ptr
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types
import Graphics.Efl.Helpers


-- | Enable or disable the map that is set
setMapEnable :: Object -> Bool -> IO ()
setMapEnable obj b = _object_transformation_enable_set obj (fromBool b)

foreign import ccall "evas_object_map_enable_set" _object_transformation_enable_set :: Object -> EinaBool -> IO ()

-- | Enable transformation on the given object
enableMap :: Object -> IO ()
enableMap obj = setMapEnable obj True

-- | Disable transformation on the given object
disableMap :: Object -> IO ()
disableMap obj = setMapEnable obj False

-- | Get the map enabled state
isMapEnabled :: Object -> IO Bool
isMapEnabled obj = toBool <$> _object_transformation_enable_get obj

foreign import ccall "evas_object_map_enable_get" _object_transformation_enable_get :: Object -> IO EinaBool


-- | Set current object transformation
setMap :: Map -> Object -> IO ()
setMap = flip _setMap

foreign import ccall "evas_object_map_set" _setMap :: Object -> Map -> IO ()

-- | Get current object transformation
foreign import ccall "evas_object_map_get" getMap :: Object -> IO Map

-- | Populate source and destination map points to match exactly object
populateMapPointsFromObjectFull :: Object -> Coord -> Map -> IO ()
populateMapPointsFromObjectFull obj xy m = _populateMapPointsFromObjectFull m obj xy

foreign import ccall "evas_map_util_points_populate_from_object_full" _populateMapPointsFromObjectFull :: Map -> Object -> Coord -> IO ()

-- | Populate source and destination map points to match exactly object
populateMapPointsFromObject :: Object -> Map -> IO ()
populateMapPointsFromObject = flip _populateMapPointsFromObject

foreign import ccall "evas_map_util_points_populate_from_object" _populateMapPointsFromObject :: Map -> Object -> IO ()

-- | Populate source and destination map points to match given geometry
populateMapPointsFromGeometry :: Coord -> Coord -> Coord -> Coord -> Coord -> Map -> IO ()
populateMapPointsFromGeometry x y z w h m = _populateMapPointsFromGeometry m x y z w h

foreign import ccall "evas_map_util_points_populate_from_geometry" _populateMapPointsFromGeometry :: Map -> Coord -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Set color of all points to given color
setMapPointsColor :: Int -> Int -> Int -> Int -> Map -> IO ()
setMapPointsColor r g b a m = _setMapPointsColor m r g b a

foreign import ccall "evas_map_util_points_color_set" _setMapPointsColor :: Map -> Int -> Int -> Int -> Int -> IO ()

-- | Change the transformation to apply the given rotation
rotateMap :: Double -> Coord -> Coord -> Map -> IO ()
rotateMap phi cx cy m = _rotateMap m phi cx cy

foreign import ccall "evas_map_util_rotate" _rotateMap :: Map -> Double -> Coord -> Coord -> IO ()

-- | Change the transformation to apply the given zoom
zoomMap :: Double -> Double -> Coord -> Coord -> Map -> IO ()
zoomMap fx fy x y m = _zoomMap m fx fy x y

foreign import ccall "evas_map_util_zoom" _zoomMap :: Map -> Double -> Double -> Coord -> Coord -> IO ()

-- | Rotate the map around 3 axes in 3D
rotateMap3D :: Double -> Double -> Double -> Coord -> Coord -> Coord -> Map -> IO ()
rotateMap3D rx ry rz cx cy cz m = _rotateMap3D m rx ry rz cx cy cz

foreign import ccall "evas_map_util_3d_rotate" _rotateMap3D :: Map -> Double -> Double -> Double -> Coord -> Coord -> Coord -> IO ()

-- | Rotate the map in 3D using a unit quaternion
--foreign import ccall "evas_map_util_quat_rotate" rotateMapQuat :: Map -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

-- | Perform lighting calculations on the given Map
lighting3DMap :: Coord -> Coord -> Int -> Int -> Int -> Int -> Int -> Int -> Map -> IO ()
lighting3DMap x y a b c d e f m = _lighting3DMap m x y a b c d e f

foreign import ccall "evas_map_util_3d_lighting" _lighting3DMap :: Map -> Coord -> Coord -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

-- | Apply a perspective transform to the map
perspective3DMap :: Coord -> Coord -> Coord -> Coord -> Map -> IO ()
perspective3DMap x y x' y' m = _perspective3DMap m x y x' y'

foreign import ccall "evas_map_util_3d_perspective" _perspective3DMap :: Map -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the clockwise state of a map
isMapClockwise :: Map -> IO Bool
isMapClockwise tr = toBool <$> _transformation_util_clockwise_get tr

foreign import ccall "evas_map_util_clockwise_get" _transformation_util_clockwise_get :: Map -> IO EinaBool

-- | Create map of transformation points to be later used with an Evas object
foreign import ccall "evas_map_new" createMap :: Int -> IO Map

-- | Set the smoothing for map rendering
setMapSmooth :: Bool -> Map -> IO ()
setMapSmooth b m = _transformation_smooth_set m (fromBool b)

foreign import ccall "evas_map_smooth_set" _transformation_smooth_set :: Map -> EinaBool -> IO ()

-- | Get the smoothing for map rendering
isMapSmooth :: Map -> IO Bool
isMapSmooth tr = toBool <$> _transformation_smooth_get tr

foreign import ccall "evas_map_smooth_get" _transformation_smooth_get :: Map -> IO EinaBool

-- | Set the alpha flag for map rendering
setMapAlpha :: Bool -> Map -> IO ()
setMapAlpha b m = _transformation_alpha_set m (fromBool b)

foreign import ccall "evas_map_alpha_set" _transformation_alpha_set :: Map -> EinaBool -> IO ()

-- | Get the alpha flag for map rendering
isMapAlpha :: Map -> IO Bool
isMapAlpha tr = toBool <$> _transformation_alpha_get tr

foreign import ccall "evas_map_alpha_get" _transformation_alpha_get :: Map -> IO EinaBool

-- | Copy a previously allocated map
foreign import ccall "evas_map_dup" dupMap :: Map -> IO Map

-- | Free a previously allocated map
foreign import ccall "evas_map_free" freeMap :: Map -> IO ()

-- | Get a maps size
foreign import ccall "evas_map_count_get" getMapCount :: Map -> IO Int

-- | Change the map point's coordinate
setMapPointCoord :: Int -> Coord -> Coord -> Coord -> Map -> IO ()
setMapPointCoord n x y z m = _setMapPointCoord m n x y z

foreign import ccall "evas_map_point_coord_set" _setMapPointCoord :: Map -> Int -> Coord -> Coord -> Coord -> IO ()

-- | Get the map point's coordinate
getMapPointCoord :: Int -> Map -> IO (Coord, Coord, Coord)
getMapPointCoord n m = get3_helper (_transformation_point_coord_get m n)

foreign import ccall "evas_map_point_coord_get" _transformation_point_coord_get :: Map -> Int -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()

-- | Change the map point's U and V texture source point
setMapPointImageUV :: Int -> Double -> Double -> Map -> IO ()
setMapPointImageUV n u v m = _setMapPointImageUV m n u v

foreign import ccall "evas_map_point_image_uv_set" _setMapPointImageUV :: Map -> Int -> Double -> Double -> IO ()

-- | Get the map point's U and V texture source points
getMapPointImageUV :: Map -> Int -> IO (Double, Double)
getMapPointImageUV tr idx = get2_helper (_transformation_point_image_uv_get tr idx)

foreign import ccall "evas_map_point_image_uv_get" _transformation_point_image_uv_get :: Map -> Int -> Ptr Double -> Ptr Double -> IO ()

-- | Set the color of a vertex in the map
setMapPointColor :: Int -> Int -> Int -> Int -> Int -> Map -> IO ()
setMapPointColor n r g b a m = _setMapPointColor m n r g b a

foreign import ccall "evas_map_point_color_set" _setMapPointColor :: Map -> Int -> Int -> Int -> Int -> Int -> IO ()


-- | Get the color set on a vertex in the map
getMapPointColor :: Map -> Int -> IO (Int, Int, Int, Int)
getMapPointColor tr idx = get4_helper (_transformation_point_color_get tr idx)

foreign import ccall "evas_map_point_color_get" _transformation_point_color_get :: Map -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
