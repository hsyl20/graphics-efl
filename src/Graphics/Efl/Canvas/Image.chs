{-# Language ForeignFunctionInterface #-}

-- | Image
module Graphics.Efl.Canvas.Image (
   addImage, addFilledImage,
   setImageMemFile, setImageFile, getImageFile,
   setImageBorder, getImageBorder,
   setImageBorderCenterFill, getImageBorderCenterFill,
   setImageFilling, isImageFilled,
   setImageBorderScale, getImageBorderScale,
   setImageFill, getImageFill,
   setImageFillSpread, getImageFillSpread,
   setImageSize, getImageSize,
   getImageStride,
   getImageLoadError, 
   setImageData, getImageData, convertImageData,
   setImageDataCopy, addImageDataUpdate,
   setImageAlpha, getImageAlpha,
   setImageSmoothScale, getImageSmoothScale,
   preloadImage, reloadImage, saveImage,
   importImagePixels, setImagePixelsGetCallback,
   setImageDirtyPixels, getImageDirtyPixels,
   setImageLoadDPI, getImageLoadDPI,
   setImageLoadSize, getImageLoadSize,
   setImageLoadScaleDownFactor, getImageLoadScaleDownFactor,
   setImageLoadRegion, getImageLoadRegion,
   setImageLoadOrientation, getImageLoadOrientation,
   setImageColorSpace, getImageColorSpace,
   getImageRegionSupport,
   setImageNativeSurface, getImageNativeSurface,
   setImageVideoSurface, getImageVideoSurface,
   setImageScaleHint, getImageScaleHint,
   setImageContentHint, getImageContentHint,
   setImageAlphaMask,
   setImageSource, getImageSource, unsetImageSource,
   canLoadImageExtension, canLoadImageExtensionFast,
   isImageAnimated, getAnimatedImageLoopType,
   getAnimatedImageLoopCount, getAnimatedImageFrameCount,
   getAnimatedImageFrameDuration, setAnimatedImageFrame
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Helpers
import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types


-- | Create a new image object on the given Evas e canvas
foreign import ccall "evas_object_image_add" addImage :: Canvas -> IO Object

-- | Create a new image object that automatically scales its bound image to the object's area, on both axis
foreign import ccall "evas_object_image_filled_add" addFilledImage :: Canvas -> IO Object



-- | Set the data for an image from memory to be loaded
foreign import ccall "evas_object_image_memfile_set" setImageMemFile :: Object -> Ptr () -> Int -> CString -> CString -> IO ()

-- | Set the source file from where an image object must fetch the real image data (it may be an Eet file, besides pure image ones)
setImageFile :: String -> Maybe String -> Object -> IO ()
setImageFile file key obj = 
   withCString file $ \file' -> case key of
      Nothing -> _setImageFile obj file' nullPtr
      Just k -> withCString k (\key' -> _setImageFile obj file' key')

foreign import ccall "evas_object_image_file_set" _setImageFile :: Object -> CString -> CString -> IO ()

-- | Retrieve the source file from where an image object is to fetch the real image data (it may be an Eet file, besides pure image ones)
foreign import ccall "evas_object_image_file_get" getImageFile :: Object -> Ptr CString -> CString -> IO ()



-- | Set the dimensions for an image object's border, a region which won't ever be scaled together with its center
foreign import ccall "evas_object_image_border_set" setImageBorder :: Object -> Int -> Int -> Int -> Int -> IO ()

-- | Retrieve the dimensions for an image object's border, a region which won't ever be scaled together with its center
getImageBorder :: Object -> IO (Int,Int,Int,Int)
getImageBorder obj = get4_helper (_object_image_border_get obj)

foreign import ccall "evas_object_image_border_get" _object_image_border_get :: Object -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

-- | Set how the center part of the given image object (not the borders) should be drawn when Evas is rendering it
setImageBorderCenterFill :: Object -> BorderFillMode -> IO ()
setImageBorderCenterFill obj mode = _object_image_border_center_fill_set obj (fromEnum mode)

foreign import ccall "evas_object_image_border_center_fill_set" _object_image_border_center_fill_set :: Object -> Int -> IO ()

-- | Retrieve how the center part of the given image object (not the borders) is to be drawn when Evas is rendering it
getImageBorderCenterFill :: Object -> IO BorderFillMode
getImageBorderCenterFill obj = toEnum <$> _object_image_border_center_fill_get obj

foreign import ccall "evas_object_image_border_center_fill_get" _object_image_border_center_fill_get :: Object -> IO Int



-- | Set whether the image object's fill property should track the object's size
setImageFilling :: Object -> Bool -> IO ()
setImageFilling obj b = _object_image_filled_set obj (fromBool b)

foreign import ccall "evas_object_image_filled_set" _object_image_filled_set :: Object -> EinaBool -> IO ()

-- | Retrieve whether the image object's fill property should track the object's size
isImageFilled :: Object -> IO Bool
isImageFilled obj = toBool <$> _object_image_filled_get obj

foreign import ccall "evas_object_image_filled_get" _object_image_filled_get :: Object -> IO EinaBool


-- | Set the scaling factor (multiplier) for the borders of an image object
foreign import ccall "evas_object_image_border_scale_set" setImageBorderScale :: Object -> Double -> IO ()

-- | Retrieve the scaling factor (multiplier) for the borders of an image object
foreign import ccall "evas_object_image_border_scale_get" getImageBorderScale :: Object -> IO Double


-- | Set how to fill an image object's drawing rectangle given the (real) image bound to it
setImageFill :: Coord -> Coord -> Coord -> Coord -> Object -> IO ()
setImageFill x y w h obj = _setImageFill obj x y w h

foreign import ccall "evas_object_image_fill_set" _setImageFill :: Object -> Coord -> Coord -> Coord -> Coord -> IO ()

-- | Retrieve how an image object is to fill its drawing rectangle, given the (real) image bound to it
getImageFill :: Object -> IO (Coord,  Coord, Coord, Coord)
getImageFill obj = get4_helper (_object_image_fill_get obj)

foreign import ccall "evas_object_image_fill_get" _object_image_fill_get :: Object -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()




-- | Sets the tiling mode for the given evas image object's fill
setImageFillSpread :: Object -> FillSpread -> IO ()
setImageFillSpread obj mode = _object_image_fill_spread_set obj (fromEnum mode)

foreign import ccall "evas_object_image_fill_spread_set" _object_image_fill_spread_set :: Object -> Int -> IO ()

-- | Retrieve the spread (tiling mode) for the given image object's fill
getImageFillSpread :: Object -> IO FillSpread
getImageFillSpread obj = toEnum <$> _object_image_fill_spread_get obj

foreign import ccall "evas_object_image_fill_spread_get" _object_image_fill_spread_get :: Object -> IO Int




-- | Set the size of the given image object
foreign import ccall "evas_object_image_size_set" setImageSize :: Object -> Int -> Int -> IO ()

-- | Retrieve the size of the given image object
getImageSize :: Object -> IO (Int,Int)
getImageSize obj = get2_helper (_object_image_size_get obj)

foreign import ccall "evas_object_image_size_get" _object_image_size_get :: Object -> Ptr Int -> Ptr Int -> IO ()



-- | Retrieve the row stride of the given image object
foreign import ccall "evas_object_image_stride_get" getImageStride :: Object -> IO Int



-- | Retrieve a number representing any error that occurred during the last loading of the given image object's source image
getImageLoadError :: Object -> IO LoadError
getImageLoadError obj = toEnum <$> _object_image_load_error_get obj

foreign import ccall "evas_object_image_load_error_get" _object_image_load_error_get :: Object -> IO Int



-- | Set the raw image data of the given image object
foreign import ccall "evas_object_image_data_set" setImageData :: Object -> Ptr () -> IO ()



-- | Get a pointer to the raw image data of the given image object
getImageData :: Object -> Bool -> IO (Ptr ())
getImageData obj for_writing = _object_image_data_get obj (fromBool for_writing)

foreign import ccall "evas_object_image_data_get" _object_image_data_get :: Object -> EinaBool -> IO (Ptr ())



-- | Convert the raw image data of the given image object to the specified colorspace
convertImageData :: Object -> ColorSpace -> IO (Ptr ())
convertImageData obj to_cspace = _object_image_data_convert obj (fromEnum to_cspace)

foreign import ccall "evas_object_image_data_convert" _object_image_data_convert :: Object -> Int -> IO (Ptr ())




-- | Replace the raw image data of the given image object
foreign import ccall "evas_object_image_data_copy_set" setImageDataCopy :: Object -> Ptr () -> IO ()



-- | Mark a sub-region of the given image object to be redrawn
foreign import ccall "evas_object_image_data_update_add" addImageDataUpdate :: Object -> Int -> Int -> Int -> Int -> IO ()



-- | Enable or disable alpha channel usage on the given image object
setImageAlpha :: Object -> Bool -> IO ()
setImageAlpha obj has_alpha = _object_image_alpha_set obj (fromBool has_alpha)

foreign import ccall "evas_object_image_alpha_set" _object_image_alpha_set :: Object -> EinaBool -> IO ()


-- | Retrieve whether alpha channel data is being used on the given image object
getImageAlpha :: Object -> IO Bool
getImageAlpha obj = toBool <$> _object_image_alpha_get obj

foreign import ccall "evas_object_image_alpha_get" _object_image_alpha_get :: Object -> IO EinaBool



-- | Set whether to use high-quality image scaling algorithm on the given image object
setImageSmoothScale :: Object -> Bool -> IO ()
setImageSmoothScale obj scale = _object_image_smooth_scale_set obj (fromBool scale)

foreign import ccall "evas_object_image_smooth_scale_set" _object_image_smooth_scale_set :: Object -> EinaBool -> IO ()


-- | Retrieve whether the given image object is using high-quality image scaling algorithm
getImageSmoothScale :: Object -> IO Bool
getImageSmoothScale obj = toBool <$> _object_image_smooth_scale_get obj

foreign import ccall "evas_object_image_smooth_scale_get" _object_image_smooth_scale_get :: Object -> IO EinaBool




-- | Preload an image object's image data in the background
preloadImage :: Object -> Bool -> IO ()
preloadImage obj cancel = _object_image_preload obj (fromBool cancel)

foreign import ccall "evas_object_image_preload" _object_image_preload :: Object -> EinaBool -> IO ()



-- | Reload an image object's image data
foreign import ccall "evas_object_image_reload" reloadImage :: Object -> IO ()



-- | Save the given image object's contents to an (image) file
saveImage :: Object -> String -> String -> String -> IO Bool
saveImage obj file key flags = 
   withCString file $ \cfile ->
   withCString key $ \ckey ->
   withCString flags $ \cflags -> toBool <$> _object_image_save obj cfile ckey cflags

foreign import ccall "evas_object_image_save" _object_image_save :: Object -> CString -> CString -> CString -> IO EinaBool

-- | Import pixels from given source to a given canvas image object
foreign import ccall "evas_object_image_pixels_import" importImagePixels :: Object -> PixelImportSource -> IO EinaBool

-- | Set the callback function to get pixels from a canvas' image
foreign import ccall "evas_object_image_pixels_get_callback_set" setImagePixelsGetCallback :: Object -> ObjectImagePixelsGetCb -> Ptr () -> IO ()

-- | Mark whether the given image object is dirty and needs to request its pixels
setImageDirtyPixels :: Object -> Bool -> IO ()
setImageDirtyPixels obj b = _object_image_pixels_dirty_set obj (fromBool b)

foreign import ccall "evas_object_image_pixels_dirty_set" _object_image_pixels_dirty_set :: Object -> EinaBool -> IO ()

-- | Retrieve whether the given image object is dirty (needs to be redrawn)
getImageDirtyPixels :: Object -> IO Bool
getImageDirtyPixels obj = toBool <$> _object_image_pixels_dirty_get obj

foreign import ccall "evas_object_image_pixels_dirty_get" _object_image_pixels_dirty_get :: Object -> IO EinaBool

-- | Set the DPI resolution of an image object's source image
foreign import ccall "evas_object_image_load_dpi_set" setImageLoadDPI :: Object -> Double -> IO ()

-- | Get the DPI resolution of a loaded image object in the canvas
foreign import ccall "evas_object_image_load_dpi_get" getImageLoadDPI :: Object -> IO Double

-- | Set the size of a given image object's source image, when loading it
foreign import ccall "evas_object_image_load_size_set" setImageLoadSize :: Object -> Int -> Int -> IO ()

-- | Get the size of a given image object's source image, when loading it
getImageLoadSize :: Object -> IO (Int,Int)
getImageLoadSize obj = get2_helper (_object_image_load_size_get obj)

foreign import ccall "evas_object_image_load_size_get" _object_image_load_size_get :: Object -> Ptr Int -> Ptr Int -> IO ()


-- | Set the scale down factor of a given image object's source image, when loading it
foreign import ccall "evas_object_image_load_scale_down_set" setImageLoadScaleDownFactor :: Object -> Int -> IO ()

-- | Get the scale down factor of a given image object's source image, when loading it
foreign import ccall "evas_object_image_load_scale_down_get" getImageLoadScaleDownFactor :: Object -> IO Int


-- | Inform a given image object to load a selective region of its source image
foreign import ccall "evas_object_image_load_region_set" setImageLoadRegion :: Object -> Int -> Int -> IO ()

-- | Retrieve the coordinates of a given image object's selective (source image) load region.
getImageLoadRegion :: Object -> IO (Int,Int,Int,Int)
getImageLoadRegion obj = get4_helper (_object_image_load_region_get obj)

foreign import ccall "evas_object_image_load_region_get" _object_image_load_region_get :: Object -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()


-- | Define if the orientation information in the image file should be honored
setImageLoadOrientation :: Object -> Bool -> IO ()
setImageLoadOrientation obj b = _object_image_load_orientation_set obj (fromBool b)

foreign import ccall "evas_object_image_load_orientation_set" _object_image_load_orientation_set :: Object -> EinaBool -> IO ()

-- | Get if the orientation information in the image file should be honored
getImageLoadOrientation :: Object -> IO Bool
getImageLoadOrientation obj = toBool <$> _object_image_load_orientation_get obj

foreign import ccall "evas_object_image_load_orientation_get" _object_image_load_orientation_get :: Object -> IO EinaBool


-- | Set the colorspace of a given image of the canvas
setImageColorSpace :: Object -> ColorSpace -> IO ()
setImageColorSpace obj cspace = _object_image_colorspace_set obj (fromEnum cspace)

foreign import ccall "evas_object_image_colorspace_set" _object_image_colorspace_set :: Object -> Int -> IO ()

-- | Get the colorspace of a given image of the canvas
getImageColorSpace :: Object -> IO ColorSpace
getImageColorSpace obj = toEnum <$> _object_image_colorspace_get obj

foreign import ccall "evas_object_image_colorspace_get" _object_image_colorspace_get :: Object -> IO Int


-- | Get the support state of a given image
getImageRegionSupport :: Object -> IO Bool
getImageRegionSupport obj = toBool <$> _object_image_region_support_get obj

foreign import ccall "evas_object_image_region_support_get" _object_image_region_support_get :: Object -> IO EinaBool


-- | Set the native surface of a given image of the canvas
foreign import ccall "evas_object_image_native_surface_set" setImageNativeSurface :: Object -> NativeSurface -> IO ()

-- | Get the native surface of a given image of the canvas
foreign import ccall "evas_object_image_native_surface_get" getImageNativeSurface :: Object -> IO NativeSurface

-- | Set the video surface linked to a given image of the canvas
foreign import ccall "evas_object_image_video_surface_set" setImageVideoSurface :: Object -> VideoSurface -> IO ()

-- | Get the video surface linekd to a given image of the canvas
foreign import ccall "evas_object_image_video_surface_get" getImageVideoSurface :: Object -> IO VideoSurface


-- | Set the scale hint of a given image of the canvas
setImageScaleHint :: Object -> ImageScaleHint -> IO ()
setImageScaleHint obj hint = _object_image_scale_hint_set obj (fromEnum hint)

foreign import ccall "evas_object_image_scale_hint_set" _object_image_scale_hint_set :: Object -> Int -> IO ()

-- | Get the scale hint of a given image of the canvas
getImageScaleHint :: Object -> IO ImageScaleHint
getImageScaleHint obj = toEnum <$> _object_image_scale_hint_get obj

foreign import ccall "evas_object_image_scale_hint_get" _object_image_scale_hint_get :: Object -> IO Int


-- | Set the content hint setting of a given image object of the canvas
setImageContentHint :: Object -> ImageContentHint -> IO ()
setImageContentHint obj hint = _object_image_content_hint_set obj (fromEnum hint)

foreign import ccall "evas_object_image_content_hint_set" _object_image_content_hint_set :: Object -> Int -> IO ()

-- | Get the content hint setting of a given image object of the canvas 
getImageContentHint :: Object -> IO ImageContentHint
getImageContentHint obj = toEnum <$> _object_image_content_hint_get obj

foreign import ccall "evas_object_image_content_hint_get" _object_image_content_hint_get :: Object -> IO Int


-- | Enable an image to be used as an alpha mask
setImageAlphaMask:: Object -> Bool -> IO ()
setImageAlphaMask obj ismask = _object_image_alpha_mask_set obj (fromBool ismask)

foreign import ccall "evas_object_image_alpha_mask_set" _object_image_alpha_mask_set :: Object -> EinaBool -> IO ()


-- | Set the source object on an image object to used as a proxy
foreign import ccall "evas_object_image_source_set" setImageSource :: Object -> Object -> IO ()

-- | Get the current source object of an image object
foreign import ccall "evas_object_image_source_get" getImageSource :: Object -> IO Object 

-- | Clear the source object on a proxy image object
unsetImageSource :: Object -> IO Bool
unsetImageSource obj = toBool <$> _object_image_source_unset obj

foreign import ccall "evas_object_image_source_unset" _object_image_source_unset :: Object -> IO EinaBool


-- | Check if a file extension may be supported by Image Object Functions
canLoadImageExtension :: String -> IO Bool
canLoadImageExtension file = toBool <$> withCString file _object_image_extension_can_load_get

foreign import ccall "evas_object_image_extension_can_load_get" _object_image_extension_can_load_get :: CString -> IO EinaBool

-- | Check if a file extension may be supported by Image Object Functions
canLoadImageExtensionFast :: String -> IO Bool
canLoadImageExtensionFast file = toBool <$> withCString file _object_image_extension_can_load_fast_get

foreign import ccall "evas_object_image_extension_can_load_fast_get" _object_image_extension_can_load_fast_get :: CString -> IO EinaBool

-- | Check if an image object can be animated (have multiple frames)
isImageAnimated :: Object -> IO Bool
isImageAnimated obj = toBool <$> _object_image_animated_get obj

foreign import ccall "evas_object_image_animated_get" _object_image_animated_get :: Object -> IO EinaBool

-- | Get the total number of frames of the image object
foreign import ccall "evas_object_image_animated_frame_count_get" getAnimatedImageFrameCount :: Object -> IO Int


-- | Get the kind of looping the image object does
getAnimatedImageLoopType :: Object -> IO ImageAnimatedLoopType
getAnimatedImageLoopType obj = toEnum <$> _object_image_animated_loop_type_get obj

foreign import ccall "evas_object_image_animated_loop_type_get" _object_image_animated_loop_type_get :: Object -> IO Int


-- | Get the number times the animation of the object loops
foreign import ccall "evas_object_image_animated_loop_count_get" getAnimatedImageLoopCount :: Object -> IO Int

-- | Get the duration of a sequence of frames
foreign import ccall "evas_object_image_animated_frame_duration_get" getAnimatedImageFrameDuration :: Object -> Int -> Int -> IO Double

-- | Set the frame to current frame of an image object
foreign import ccall "evas_object_image_animated_frame_set" setAnimatedImageFrame :: Object -> Int -> IO ()
