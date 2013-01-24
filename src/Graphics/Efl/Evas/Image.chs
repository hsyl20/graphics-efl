{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Evas.Image where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Helpers
import Graphics.Efl.Eina
import Graphics.Efl.Evas.Types


-- | Create a new image object on the given Evas e canvas
foreign import ccall "evas_object_image_add" object_image_add :: Evas -> IO Object

-- | Create a new image object that automatically scales its bound image to the object's area, on both axis
foreign import ccall "evas_object_image_filled_add" object_image_filled_add :: Evas -> IO Object



-- | Set the data for an image from memory to be loaded
foreign import ccall "evas_object_image_memfile_set" object_image_memfile_set :: Object -> Ptr () -> Int -> CString -> CString -> IO ()

-- | Set the source file from where an image object must fetch the real image data (it may be an Eet file, besides pure image ones)
foreign import ccall "evas_object_image_file_set" object_image_file_set :: Object -> CString -> CString -> IO ()

-- | Retrieve the source file from where an image object is to fetch the real image data (it may be an Eet file, besides pure image ones)
foreign import ccall "evas_object_image_file_get" object_image_file_get :: Object -> Ptr CString -> CString -> IO ()



-- | Set the dimensions for an image object's border, a region which won't ever be scaled together with its center
foreign import ccall "evas_object_image_border_set" object_image_border_set :: Object -> Int -> Int -> Int -> Int -> IO ()

-- | Retrieve the dimensions for an image object's border, a region which won't ever be scaled together with its center
object_image_border_get :: Object -> IO (Int,Int,Int,Int)
object_image_border_get obj = get4_helper (_object_image_border_get obj)

foreign import ccall "evas_object_image_border_get" _object_image_border_get :: Object -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

-- | Set how the center part of the given image object (not the borders) should be drawn when Evas is rendering it
object_image_border_center_fill_set :: Object -> BorderFillMode -> IO ()
object_image_border_center_fill_set obj mode = _object_image_border_center_fill_set obj (fromEnum mode)

foreign import ccall "evas_object_image_border_center_fill_set" _object_image_border_center_fill_set :: Object -> Int -> IO ()

-- | Retrieve how the center part of the given image object (not the borders) is to be drawn when Evas is rendering it
object_image_border_center_fill_get :: Object -> IO BorderFillMode
object_image_border_center_fill_get obj = toEnum <$> _object_image_border_center_fill_get obj

foreign import ccall "evas_object_image_border_center_fill_get" _object_image_border_center_fill_get :: Object -> IO Int



-- | Set whether the image object's fill property should track the object's size
object_image_filled_set :: Object -> Bool -> IO ()
object_image_filled_set obj b = _object_image_filled_set obj (fromBool b)

foreign import ccall "evas_object_image_filled_set" _object_image_filled_set :: Object -> EinaBool -> IO ()

-- | Retrieve whether the image object's fill property should track the object's size
object_image_filled_get :: Object -> IO Bool
object_image_filled_get obj = toBool <$> _object_image_filled_get obj

foreign import ccall "evas_object_image_filled_get" _object_image_filled_get :: Object -> IO EinaBool


-- | Set the scaling factor (multiplier) for the borders of an image object
foreign import ccall "evas_object_image_border_scale_set" object_image_border_scale_set :: Object -> Double -> IO ()

-- | Retrieve the scaling factor (multiplier) for the borders of an image object
foreign import ccall "evas_object_image_border_scale_get" object_image_border_scale_get :: Object -> IO Double


-- | Set how to fill an image object's drawing rectangle given the (real) image bound to it
foreign import ccall "evas_object_image_fill_set" object_image_fill_set :: Object -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Retrieve how an image object is to fill its drawing rectangle, given the (real) image bound to it
object_image_fill_get :: Object -> IO (Coord,  Coord, Coord, Coord)
object_image_fill_get obj = get4_helper (_object_image_fill_get obj)

foreign import ccall "evas_object_image_fill_get" _object_image_fill_get :: Object -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()




-- | Sets the tiling mode for the given evas image object's fill
object_image_fill_spread_set :: Object -> FillSpread -> IO ()
object_image_fill_spread_set obj mode = _object_image_fill_spread_set obj (fromEnum mode)

foreign import ccall "evas_object_image_fill_spread_set" _object_image_fill_spread_set :: Object -> Int -> IO ()

-- | Retrieve the spread (tiling mode) for the given image object's fill
object_image_fill_spread_get :: Object -> IO FillSpread
object_image_fill_spread_get obj = toEnum <$> _object_image_fill_spread_get obj

foreign import ccall "evas_object_image_fill_spread_get" _object_image_fill_spread_get :: Object -> IO Int




-- | Set the size of the given image object
foreign import ccall "evas_object_image_size_set" object_image_size_set :: Object -> Int -> Int -> IO ()

-- | Retrieve the size of the given image object
object_image_size_get :: Object -> IO (Int,Int)
object_image_size_get obj = get2_helper (_object_image_size_get obj)

foreign import ccall "evas_object_image_size_get" _object_image_size_get :: Object -> Ptr Int -> Ptr Int -> IO ()



-- | Retrieve the row stride of the given image object
foreign import ccall "evas_object_image_stride_get" object_image_stride_get :: Object -> IO Int



-- | Retrieve a number representing any error that occurred during the last loading of the given image object's source image
object_image_load_error_get :: Object -> IO LoadError
object_image_load_error_get obj = toEnum <$> _object_image_load_error_get obj

foreign import ccall "evas_object_image_load_error_get" _object_image_load_error_get :: Object -> IO Int



-- | Set the raw image data of the given image object
foreign import ccall "evas_object_image_data_set" object_image_data_set :: Object -> Ptr () -> IO ()



-- | Get a pointer to the raw image data of the given image object
object_image_data_get :: Object -> Bool -> IO (Ptr ())
object_image_data_get obj for_writing = _object_image_data_get obj (fromBool for_writing)

foreign import ccall "evas_object_image_data_get" _object_image_data_get :: Object -> EinaBool -> IO (Ptr ())



-- | Convert the raw image data of the given image object to the specified colorspace
object_image_data_convert :: Object -> ColorSpace -> IO (Ptr ())
object_image_data_convert obj to_cspace = _object_image_data_convert obj (fromEnum to_cspace)

foreign import ccall "evas_object_image_data_convert" _object_image_data_convert :: Object -> Int -> IO (Ptr ())




-- | Replace the raw image data of the given image object
foreign import ccall "evas_object_image_data_copy_set" object_image_data_copy_set :: Object -> Ptr () -> IO ()



-- | Mark a sub-region of the given image object to be redrawn
foreign import ccall "evas_object_image_data_update_add" object_image_data_update_add :: Object -> Int -> Int -> Int -> Int -> IO ()



-- | Enable or disable alpha channel usage on the given image object
object_image_alpha_set :: Object -> Bool -> IO ()
object_image_alpha_set obj has_alpha = _object_image_alpha_set obj (fromBool has_alpha)

foreign import ccall "evas_object_image_alpha_set" _object_image_alpha_set :: Object -> EinaBool -> IO ()


-- | Retrieve whether alpha channel data is being used on the given image object
object_image_alpha_get :: Object -> IO Bool
object_image_alpha_get obj = toBool <$> _object_image_alpha_get obj

foreign import ccall "evas_object_image_alpha_get" _object_image_alpha_get :: Object -> IO EinaBool



-- | Set whether to use high-quality image scaling algorithm on the given image object
object_image_smooth_scale_set :: Object -> Bool -> IO ()
object_image_smooth_scale_set obj scale = _object_image_smooth_scale_set obj (fromBool scale)

foreign import ccall "evas_object_image_smooth_scale_set" _object_image_smooth_scale_set :: Object -> EinaBool -> IO ()


-- | Retrieve whether the given image object is using high-quality image scaling algorithm
object_image_smooth_scale_get :: Object -> IO Bool
object_image_smooth_scale_get obj = toBool <$> _object_image_smooth_scale_get obj

foreign import ccall "evas_object_image_smooth_scale_get" _object_image_smooth_scale_get :: Object -> IO EinaBool




-- | Preload an image object's image data in the background
object_image_preload :: Object -> Bool -> IO ()
object_image_preload obj cancel = _object_image_preload obj (fromBool cancel)

foreign import ccall "evas_object_image_preload" _object_image_preload :: Object -> EinaBool -> IO ()



-- | Reload an image object's image data
foreign import ccall "evas_object_image_reload" object_image_reload :: Object -> IO ()
