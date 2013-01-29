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
object_map_enable_set :: Object -> Bool -> IO ()
object_map_enable_set obj b = _object_map_enable_set obj (fromBool b)

foreign import ccall "evas_object_map_enable_set" _object_map_enable_set :: Object -> EinaBool -> IO ()

-- | Get the map enabled state
object_map_enable_get :: Object -> IO Bool
object_map_enable_get obj = toBool <$> _object_map_enable_get obj

foreign import ccall "evas_object_map_enable_get" _object_map_enable_get :: Object -> IO EinaBool


-- | Set current object transformation
foreign import ccall "evas_object_map_set" object_transformation_set :: Object -> Transformation -> IO ()

-- | Get current object transformation
foreign import ccall "evas_object_map_get" object_transformation_get :: Object -> IO Transformation

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object_full" transformation_util_points_populate_from_object_full :: Transformation -> Object -> Coord -> IO ()

-- | Populate source and destination map points to match exactly object
foreign import ccall "evas_map_util_points_populate_from_object" transformation_util_points_populate_from_object :: Transformation -> Object -> IO ()

-- | Populate source and destination map points to match given geometry
foreign import ccall "evas_map_util_points_populate_from_geometry" transformation_util_points_populate_from_geometry :: Transformation -> Coord -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Set color of all points to given color
foreign import ccall "evas_map_util_points_color_set" transformation_util_points_color_set :: Transformation -> Int -> Int -> Int -> Int -> IO ()

-- | Change the transformation to apply the given rotation
foreign import ccall "evas_map_util_rotate" transformation_util_rotate :: Transformation -> Double -> Coord -> Coord -> IO ()

-- | Change the transformation to apply the given zoom
foreign import ccall "evas_map_util_zoom" transformation_util_zoom :: Transformation -> Double -> Double -> Coord -> Coord -> IO ()

-- | Rotate the map around 3 axes in 3D
foreign import ccall "evas_map_util_3d_rotate" transformation_util_3d_rotate :: Transformation -> Double -> Double -> Double -> Coord -> Coord -> Coord -> IO ()

-- | Rotate the map in 3D using a unit quaternion
foreign import ccall "evas_map_util_quat_rotate" transformation_util_quat_rotate :: Transformation -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

-- | Perform lighting calculations on the given Map
foreign import ccall "evas_map_util_3d_lightning" transformation_util_3d_lightning :: Transformation -> Coord -> Coord -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

-- | Apply a perspective transform to the map
foreign import ccall "evas_map_util_3d_perspective" transformation_util_3d_perspective :: Transformation -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Get the clockwise state of a map
transformation_util_clockwise_get :: Transformation -> IO Bool
transformation_util_clockwise_get tr = toBool <$> _transformation_util_clockwise_get tr

foreign import ccall "evas_map_util_clockwise_get" _transformation_util_clockwise_get :: Transformation -> IO EinaBool

-- | Create map of transformation points to be later used with an Evas object
foreign import ccall "evas_map_new" transformation_new :: Int -> IO Transformation

-- | Set the smoothing for map rendering
transformation_smooth_set :: Transformation -> Bool -> IO ()
transformation_smooth_set tr b = _transformation_smooth_set tr (fromBool b)

foreign import ccall "evas_map_smooth_set" _transformation_smooth_set :: Transformation -> EinaBool -> IO ()

-- | Get the smoothing for map rendering
transformation_smooth_get :: Transformation -> IO Bool
transformation_smooth_get tr = toBool <$> _transformation_smooth_get tr

foreign import ccall "evas_map_smooth_get" _transformation_smooth_get :: Transformation -> IO EinaBool

-- | Set the alpha flag for map rendering
transformation_alpha_set :: Transformation -> Bool -> IO ()
transformation_alpha_set tr b = _transformation_alpha_set tr (fromBool b)

foreign import ccall "evas_map_alpha_set" _transformation_alpha_set :: Transformation -> EinaBool -> IO ()

-- | Get the alpha flag for map rendering
transformation_alpha_get :: Transformation -> IO Bool
transformation_alpha_get tr = toBool <$> _transformation_alpha_get tr

foreign import ccall "evas_map_alpha_get" _transformation_alpha_get :: Transformation -> IO EinaBool

-- | Copy a previously allocated map
foreign import ccall "evas_map_dup" transformation_dup :: Transformation -> IO Transformation

-- | Free a previously allocated map
foreign import ccall "evas_map_free" transformation_free :: Transformation -> IO ()

-- | Get a maps size
foreign import ccall "evas_map_count_get" transformation_count_get :: Transformation -> IO Int

-- | Change the map point's coordinate
foreign import ccall "evas_map_point_coord_set" transformation_point_coord_set :: Transformation -> Int -> Coord -> Coord -> Coord -> IO ()

-- | Get the map point's coordinate
transformation_point_coord_get :: Transformation -> Int -> IO (Coord, Coord, Coord)
transformation_point_coord_get tr idx = get3_helper (_transformation_point_coord_get tr idx)

foreign import ccall "evas_map_point_coord_get" _transformation_point_coord_get :: Transformation -> Int -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()

-- | Change the map point's U and V texture source point
foreign import ccall "evas_map_point_image_uv_set" transformation_point_image_uv_set :: Transformation -> Int -> Double -> Double -> IO ()

-- | Get the map point's U and V texture source points
transformation_point_image_uv_get :: Transformation -> Int -> IO (Double, Double)
transformation_point_image_uv_get tr idx = get2_helper (_transformation_point_image_uv_get tr idx)

foreign import ccall "evas_map_point_image_uv_get" _transformation_point_image_uv_get :: Transformation -> Int -> Ptr Double -> Ptr Double -> IO ()

-- | Set the color of a vertex in the map
foreign import ccall "evas_map_point_color_set" transformation_point_color_set :: Transformation -> Int -> Int -> Int -> Int -> Int -> IO ()


-- | Get the color set on a vertex in the map
transformation_point_color_get :: Transformation -> Int -> IO (Int, Int, Int, Int)
transformation_point_color_get tr idx = get4_helper (_transformation_point_color_get tr idx)
foreign import ccall "evas_map_point_color_get" _transformation_point_color_get :: Transformation -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
