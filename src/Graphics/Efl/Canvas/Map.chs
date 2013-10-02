{-# Language ForeignFunctionInterface #-}

-- | Transformations applicable to canvas elements
module Graphics.Efl.Canvas.Map (
   enableMap, disableMap, isMapEnabled,
   setMap, getMap,
   populateMapPointsFromObjectFull, populateMapPointsFromObject, populateMapPointsFromGeometry,
   setMapPointsColor, rotateMap, zoomMap, rotateMap3D, rotateMapQuat, lighting3DMap, perspective3DMap,
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
foreign import ccall "evas_object_map_set" setMap :: Object -> Map -> IO ()

-- | Get current object transformation
foreign import ccall "evas_object_map_get" getMap :: Object -> IO Map

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object_full" populateMapPointsFromObjectFull :: Map -> Object -> Coord -> IO ()

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object" populateMapPointsFromObject :: Map -> Object -> IO ()

-- | Populate source and destination map points to match given geometry
foreign import ccall "evas_map_util_points_populate_from_geometry" populateMapPointsFromGeometry :: Map -> Coord -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Set color of all points to given color
foreign import ccall "evas_map_util_points_color_set" setMapPointsColor :: Map -> Int -> Int -> Int -> Int -> IO ()

-- | Change the transformation to apply the given rotation
foreign import ccall "evas_map_util_rotate" rotateMap :: Map -> Double -> Coord -> Coord -> IO ()

-- | Change the transformation to apply the given zoom
foreign import ccall "evas_map_util_zoom" zoomMap :: Map -> Double -> Double -> Coord -> Coord -> IO ()

-- | Rotate the map around 3 axes in 3D
foreign import ccall "evas_map_util_3d_rotate" rotateMap3D :: Map -> Double -> Double -> Double -> Coord -> Coord -> Coord -> IO ()

-- | Rotate the map in 3D using a unit quaternion
foreign import ccall "evas_map_util_quat_rotate" rotateMapQuat :: Map -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

-- | Perform lighting calculations on the given Map
foreign import ccall "evas_map_util_3d_lighting" lighting3DMap :: Map -> Coord -> Coord -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

-- | Apply a perspective transform to the map
foreign import ccall "evas_map_util_3d_perspective" perspective3DMap :: Map -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the clockwise state of a map
isMapClockwise :: Map -> IO Bool
isMapClockwise tr = toBool <$> _transformation_util_clockwise_get tr

foreign import ccall "evas_map_util_clockwise_get" _transformation_util_clockwise_get :: Map -> IO EinaBool

-- | Create map of transformation points to be later used with an Evas object
foreign import ccall "evas_map_new" createMap :: Int -> IO Map

-- | Set the smoothing for map rendering
setMapSmooth :: Map -> Bool -> IO ()
setMapSmooth tr b = _transformation_smooth_set tr (fromBool b)

foreign import ccall "evas_map_smooth_set" _transformation_smooth_set :: Map -> EinaBool -> IO ()

-- | Get the smoothing for map rendering
isMapSmooth :: Map -> IO Bool
isMapSmooth tr = toBool <$> _transformation_smooth_get tr

foreign import ccall "evas_map_smooth_get" _transformation_smooth_get :: Map -> IO EinaBool

-- | Set the alpha flag for map rendering
setMapAlpha :: Map -> Bool -> IO ()
setMapAlpha tr b = _transformation_alpha_set tr (fromBool b)

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
foreign import ccall "evas_map_point_coord_set" setMapPointCoord :: Map -> Int -> Coord -> Coord -> Coord -> IO ()

-- | Get the map point's coordinate
getMapPointCoord :: Map -> Int -> IO (Coord, Coord, Coord)
getMapPointCoord tr idx = get3_helper (_transformation_point_coord_get tr idx)

foreign import ccall "evas_map_point_coord_get" _transformation_point_coord_get :: Map -> Int -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()

-- | Change the map point's U and V texture source point
foreign import ccall "evas_map_point_image_uv_set" setMapPointImageUV :: Map -> Int -> Double -> Double -> IO ()

-- | Get the map point's U and V texture source points
getMapPointImageUV :: Map -> Int -> IO (Double, Double)
getMapPointImageUV tr idx = get2_helper (_transformation_point_image_uv_get tr idx)

foreign import ccall "evas_map_point_image_uv_get" _transformation_point_image_uv_get :: Map -> Int -> Ptr Double -> Ptr Double -> IO ()

-- | Set the color of a vertex in the map
foreign import ccall "evas_map_point_color_set" setMapPointColor :: Map -> Int -> Int -> Int -> Int -> Int -> IO ()


-- | Get the color set on a vertex in the map
getMapPointColor :: Map -> Int -> IO (Int, Int, Int, Int)
getMapPointColor tr idx = get4_helper (_transformation_point_color_get tr idx)

foreign import ccall "evas_map_point_color_get" _transformation_point_color_get :: Map -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
