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






foreign import ccall "evas_object_pass_events_set" evas_object_pass_events_set :: Object -> Bool -> IO ()

foreign import ccall "evas_object_image_file_set" evas_object_image_file_set :: Object -> CString -> CString -> IO ()
foreign import ccall "evas_object_image_load_error_get" evas_object_image_load_error_get :: Object -> IO Int
foreign import ccall "evas_object_image_fill_set" evas_object_image_fill_set :: Object -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall "evas_object_image_size_get" evas_object_image_size_get_ :: Object -> Ptr Int -> Ptr Int -> IO ()

evas_object_image_size_get :: Object -> IO (Int,Int)
evas_object_image_size_get obj = get2_helper (evas_object_image_size_get_ obj)

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
