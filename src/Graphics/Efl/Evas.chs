{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Evas where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Control.Applicative

import Graphics.Efl.Helpers
import Graphics.Efl.Eina

#include <Evas.h>


type Object = Ptr ()
type Coord = Int

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}


{-----------------------------
 - Basic Object Manipulation -
 -----------------------------}

-- | Clip one object to another
foreign import ccall "evas_object_clip_set" object_clip_set :: Object -> Object -> IO ()

-- | Get the object clipping the given object (if any)
object_clip_get :: Object -> IO (Maybe Object)
object_clip_get obj = maybePtr <$> _object_clip_get obj

foreign import ccall "evas_object_clip_get" _object_clip_get :: Object -> IO Object
   
-- | Disable/cease clipping on a clipped object
foreign import ccall "evas_object_clip_unset" object_clip_unset :: Object -> IO ()

-- | Return a list of objects currently clipped by obj
object_clipees_get :: Object -> IO [Object]
object_clipees_get obj = toList =<< _object_clipees_get obj

foreign import ccall "evas_object_clipees_get" _object_clipees_get :: Object -> IO (EinaList Object)



-- | Set or unset a given object as the currently focused one on its canvas
object_focus_set :: Object -> Bool -> IO ()
object_focus_set obj True = _object_focus_set obj 1
object_focus_set obj False = _object_focus_set obj 0

foreign import ccall "evas_object_focus_set" _object_focus_set :: Object -> EinaBool -> IO ()

-- | Retrieve whether an object has the focus 
object_focus_get :: Object -> IO Bool
object_focus_get obj = (/= 0) <$> _object_focus_get obj

foreign import ccall "evas_object_focus_get" _object_focus_get :: Object -> IO EinaBool



-- | Sets the layer of its canvas that the given object will be part of
foreign import ccall "evas_object_layer_set" object_layer_set :: Object -> CShort -> IO ()
-- | Retrieves the layer of its canvas that the given object is part of
foreign import ccall "evas_object_layer_get" object_layer_get :: Object -> IO CShort



-- | Set the name of the given Evas object to the given name
object_name_set :: Object -> String -> IO ()
object_name_set obj name = withCString name (_object_name_set obj)

foreign import ccall "evas_object_name_set" _object_name_set :: Object -> CString -> IO ()

-- | Retrieve the name of the given Evas object
object_name_get :: Object -> IO String
object_name_get obj = peekCString =<< _object_name_get obj

foreign import ccall "evas_object_name_get" _object_name_get :: Object -> IO CString



-- | Increment object reference count to defer its deletion
foreign import ccall "evas_object_ref" object_ref :: Object -> IO ()

-- | Decrement object reference count
foreign import ccall "evas_object_unref" object_unref :: Object -> IO ()

-- | Get the object reference count
foreign import ccall "evas_object_ref_get" object_ref_get :: Object -> IO Int



-- | Mark the given Evas object for deletion (when Evas will free its memory)
foreign import ccall "evas_object_del" object_del :: Object -> IO ()



-- | Move the given Evas object to the given location inside its canvas' viewport
foreign import ccall "evas_object_move" object_move :: Object -> Coord -> Coord -> IO ()

-- | Change the size of the given Evas object
foreign import ccall "evas_object_resize" object_resize :: Object -> Coord -> Coord -> IO ()

-- | Retrieve the position and (rectangular) size of the given Evas object
object_geometry_get :: Object -> IO (Coord,Coord,Coord,Coord)
object_geometry_get obj = get4_helper (_object_geometry_get obj)

foreign import ccall "evas_object_geometry_get" _object_geometry_get :: Object -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()



-- | Make the given Evas object visible
foreign import ccall "evas_object_show" object_show :: Object -> IO ()

-- | Make the given Evas object invisible
foreign import ccall "evas_object_hide" object_hide :: Object -> IO ()

-- | Retrieve whether or not the given Evas object is visible
object_visible_get :: Object -> IO Bool
object_visible_get obj = toBool <$> object_visible_get_ obj

foreign import ccall "evas_object_visible_get" object_visible_get_ :: Object -> IO EinaBool



-- | Set the general/main color of the given Evas object to the given one
foreign import ccall "evas_object_color_set" object_color_set :: Object -> Int -> Int -> Int -> Int -> IO ()

-- | Set the general/main color of the given Evas object to the given one
object_color_get :: Object -> IO (Int,Int,Int,Int)
object_color_get obj = get4_helper (_object_color_get obj)

foreign import ccall "evas_object_color_get" _object_color_get :: Object -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()


-- | Retrieve the Evas canvas that the given object lives on
foreign import ccall "evas_object_evas_get" object_evas_get :: Object -> IO Evas


-- | Retrieve the type of the given Evas object
object_type_get :: Object -> IO String
object_type_get obj = peekCString =<< _object_type_get obj

foreign import ccall "evas_object_type_get" _object_type_get :: Object -> IO CString



-- | Raise object to the top of its layer
foreign import ccall "evas_object_raise" object_raise :: Object -> IO ()

-- | Lower object to the bottom of its layer
foreign import ccall "evas_object_lower" object_lower :: Object -> IO ()

-- | Stack an object immediately above another object
foreign import ccall "evas_object_stack_above" object_stack_above :: Object -> Object -> IO ()

-- | Stack an object immediately below another object
foreign import ccall "evas_object_stack_below" object_stack_below :: Object -> Object -> IO ()

-- | Get the Evas object stacked right above an object
object_above_get :: Object -> IO (Maybe Object)
object_above_get obj = maybePtr <$> _object_above_get obj

foreign import ccall "evas_object_above_get" _object_above_get :: Object -> IO Object

-- | Get the Evas object stacked right below an object
object_below_get :: Object -> IO (Maybe Object)
object_below_get obj = maybePtr <$> _object_below_get obj

foreign import ccall "evas_object_below_get" _object_below_get :: Object -> IO Object


{-------------
 - Rectangle -
 -------------}

-- | Add a rectangle to the given evas
foreign import ccall "evas_object_rectangle_add" evas_object_rectangle_add :: Evas -> IO Object


{---------
 - Image -
 ---------}

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
evas_object_image_size_get :: Object -> IO (Int,Int)
evas_object_image_size_get obj = get2_helper (_object_image_size_get obj)

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




foreign import ccall "evas_object_pass_events_set" evas_object_pass_events_set :: Object -> Bool -> IO ()

foreign import ccall "evas_object_image_load_error_get" evas_object_image_load_error_get :: Object -> IO Int
foreign import ccall "evas_object_image_fill_set" evas_object_image_fill_set :: Object -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall "evas_object_event_callback_add" evas_object_event_callback_add :: Object -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "wrapper" evas_object_event_wrap_callback :: (Ptr () -> Evas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb



type Evas = Ptr ()
type ObjectEventCb = FunPtr (Ptr () -> Evas -> Object -> Ptr () -> IO ())

{#enum _Evas_Callback_Type as EvasCallbackType {underscoreToCase} deriving (Eq,Show) #}

foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Evas -> Ptr Int -> Ptr Int -> IO ()

evas_output_size_get :: Evas -> IO (Int,Int)
evas_output_size_get ev = get2_helper (evas_output_size_get_ ev)


foreign import ccall "evas_event_callback_add" evas_event_callback_add :: Evas -> Int -> ObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" evas_load_error_str :: Int -> IO CString

keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
