{-# Language ForeignFunctionInterface #-}

-- | Evas "Map" functions have been renamed into "Transformation" functions
module Graphics.Efl.Canvas.Transformations where

import Foreign.Ptr
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types
import Graphics.Efl.Helpers


-- | Enable or disable the map that is set
setTransformationEnable :: Object -> Bool -> IO ()
setTransformationEnable obj b = _object_transformation_enable_set obj (fromBool b)

foreign import ccall "evas_object_map_enable_set" _object_transformation_enable_set :: Object -> EinaBool -> IO ()

enableTransformation :: Object -> IO ()
enableTransformation obj = setTransformationEnable obj True

disableTransformation :: Object -> IO ()
disableTransformation obj = setTransformationEnable obj False

-- | Get the map enabled state
isTransformationEnabled :: Object -> IO Bool
isTransformationEnabled obj = toBool <$> _object_transformation_enable_get obj

foreign import ccall "evas_object_map_enable_get" _object_transformation_enable_get :: Object -> IO EinaBool


-- | Set current object transformation
foreign import ccall "evas_object_map_set" setTransformation :: Object -> Transformation -> IO ()

-- | Get current object transformation
foreign import ccall "evas_object_map_get" getTransformation :: Object -> IO Transformation

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object_full" populateFromObjectFull :: Transformation -> Object -> Coord -> IO ()

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object" populateFromObject :: Transformation -> Object -> IO ()

-- | Populate source and destination map points to match given geometry
foreign import ccall "evas_map_util_points_populate_from_geometry" populateFromGeometry :: Transformation -> Coord -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Set color of all points to given color
foreign import ccall "evas_map_util_points_color_set" setPointsColor :: Transformation -> Int -> Int -> Int -> Int -> IO ()

-- | Change the transformation to apply the given rotation
foreign import ccall "evas_map_util_rotate" rotate :: Transformation -> Double -> Coord -> Coord -> IO ()

-- | Change the transformation to apply the given zoom
foreign import ccall "evas_map_util_zoom" zoom :: Transformation -> Double -> Double -> Coord -> Coord -> IO ()

-- | Rotate the map around 3 axes in 3D
foreign import ccall "evas_map_util_3d_rotate" rotate3D :: Transformation -> Double -> Double -> Double -> Coord -> Coord -> Coord -> IO ()

-- | Rotate the map in 3D using a unit quaternion
foreign import ccall "evas_map_util_quat_rotate" rotateQuat :: Transformation -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

-- | Perform lighting calculations on the given Map
foreign import ccall "evas_map_util_3d_lighting" lighting3D :: Transformation -> Coord -> Coord -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

-- | Apply a perspective transform to the map
foreign import ccall "evas_map_util_3d_perspective" perspective3D :: Transformation -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the clockwise state of a map
isClockwise :: Transformation -> IO Bool
isClockwise tr = toBool <$> _transformation_util_clockwise_get tr

foreign import ccall "evas_map_util_clockwise_get" _transformation_util_clockwise_get :: Transformation -> IO EinaBool

-- | Create map of transformation points to be later used with an Evas object
foreign import ccall "evas_map_new" new :: Int -> IO Transformation

-- | Set the smoothing for map rendering
setSmooth :: Transformation -> Bool -> IO ()
setSmooth tr b = _transformation_smooth_set tr (fromBool b)

foreign import ccall "evas_map_smooth_set" _transformation_smooth_set :: Transformation -> EinaBool -> IO ()

-- | Get the smoothing for map rendering
isSmooth :: Transformation -> IO Bool
isSmooth tr = toBool <$> _transformation_smooth_get tr

foreign import ccall "evas_map_smooth_get" _transformation_smooth_get :: Transformation -> IO EinaBool

-- | Set the alpha flag for map rendering
setAlpha :: Transformation -> Bool -> IO ()
setAlpha tr b = _transformation_alpha_set tr (fromBool b)

foreign import ccall "evas_map_alpha_set" _transformation_alpha_set :: Transformation -> EinaBool -> IO ()

-- | Get the alpha flag for map rendering
isAlpha :: Transformation -> IO Bool
isAlpha tr = toBool <$> _transformation_alpha_get tr

foreign import ccall "evas_map_alpha_get" _transformation_alpha_get :: Transformation -> IO EinaBool

-- | Copy a previously allocated map
foreign import ccall "evas_map_dup" duplicate :: Transformation -> IO Transformation

-- | Free a previously allocated map
foreign import ccall "evas_map_free" free :: Transformation -> IO ()

-- | Get a maps size
foreign import ccall "evas_map_count_get" getCount :: Transformation -> IO Int

-- | Change the map point's coordinate
foreign import ccall "evas_map_point_coord_set" setPointCoord :: Transformation -> Int -> Coord -> Coord -> Coord -> IO ()

-- | Get the map point's coordinate
getPointCoord :: Transformation -> Int -> IO (Coord, Coord, Coord)
getPointCoord tr idx = get3_helper (_transformation_point_coord_get tr idx)

foreign import ccall "evas_map_point_coord_get" _transformation_point_coord_get :: Transformation -> Int -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()

-- | Change the map point's U and V texture source point
foreign import ccall "evas_map_point_image_uv_set" setPointImageUV :: Transformation -> Int -> Double -> Double -> IO ()

-- | Get the map point's U and V texture source points
getPointImageUV :: Transformation -> Int -> IO (Double, Double)
getPointImageUV tr idx = get2_helper (_transformation_point_image_uv_get tr idx)

foreign import ccall "evas_map_point_image_uv_get" _transformation_point_image_uv_get :: Transformation -> Int -> Ptr Double -> Ptr Double -> IO ()

-- | Set the color of a vertex in the map
foreign import ccall "evas_map_point_color_set" setPointColor :: Transformation -> Int -> Int -> Int -> Int -> Int -> IO ()


-- | Get the color set on a vertex in the map
getPointColor :: Transformation -> Int -> IO (Int, Int, Int, Int)
getPointColor tr idx = get4_helper (_transformation_point_color_get tr idx)

foreign import ccall "evas_map_point_color_get" _transformation_point_color_get :: Transformation -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
