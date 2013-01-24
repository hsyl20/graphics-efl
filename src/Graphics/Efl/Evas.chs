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


type EvasObject = Ptr ()
type Coord = Int

{-----------------------------
 - Basic Object Manipulation -
 -----------------------------}

-- | Clip one object to another
foreign import ccall "evas_object_clip_set" object_clip_set :: EvasObject -> EvasObject -> IO ()

-- | Get the object clipping the given object (if any)
object_clip_get :: EvasObject -> IO (Maybe EvasObject)
object_clip_get obj = maybePtr <$> _object_clip_get obj

foreign import ccall "evas_object_clip_get" _object_clip_get :: EvasObject -> IO EvasObject
   
-- | Disable/cease clipping on a clipped object
foreign import ccall "evas_object_clip_unset" object_clip_unset :: EvasObject -> IO ()

-- | Return a list of objects currently clipped by obj
object_clipees_get :: EvasObject -> IO [EvasObject]
object_clipees_get obj = toList =<< _object_clipees_get obj

foreign import ccall "evas_object_clipees_get" _object_clipees_get :: EvasObject -> IO (EinaList EvasObject)



-- | Set or unset a given object as the currently focused one on its canvas
object_focus_set :: EvasObject -> Bool -> IO ()
object_focus_set obj True = _object_focus_set obj 1
object_focus_set obj False = _object_focus_set obj 0

foreign import ccall "evas_object_focus_set" _object_focus_set :: EvasObject -> EinaBool -> IO ()

-- | Retrieve whether an object has the focus 
object_focus_get :: EvasObject -> IO Bool
object_focus_get obj = (/= 0) <$> _object_focus_get obj

foreign import ccall "evas_object_focus_get" _object_focus_get :: EvasObject -> IO EinaBool



-- | Sets the layer of its canvas that the given object will be part of
foreign import ccall "evas_object_layer_set" object_layer_set :: EvasObject -> CShort -> IO ()
-- | Retrieves the layer of its canvas that the given object is part of
foreign import ccall "evas_object_layer_get" object_layer_get :: EvasObject -> IO CShort



-- | Set the name of the given Evas object to the given name
object_name_set :: EvasObject -> String -> IO ()
object_name_set obj name = withCString name (_object_name_set obj)

foreign import ccall "evas_object_name_set" _object_name_set :: EvasObject -> CString -> IO ()

-- | Retrieve the name of the given Evas object
object_name_get :: EvasObject -> IO String
object_name_get obj = peekCString =<< _object_name_get obj

foreign import ccall "evas_object_name_get" _object_name_get :: EvasObject -> IO CString



-- | Increment object reference count to defer its deletion
foreign import ccall "evas_object_ref" object_ref :: EvasObject -> IO ()

-- | Decrement object reference count
foreign import ccall "evas_object_unref" object_unref :: EvasObject -> IO ()

-- | Get the object reference count
foreign import ccall "evas_object_ref_get" object_ref_get :: EvasObject -> IO Int



-- | Mark the given Evas object for deletion (when Evas will free its memory)
foreign import ccall "evas_object_del" object_del :: EvasObject -> IO ()



-- | Move the given Evas object to the given location inside its canvas' viewport
foreign import ccall "evas_object_move" object_move :: EvasObject -> Coord -> Coord -> IO ()

-- | Change the size of the given Evas object
foreign import ccall "evas_object_resize" object_resize :: EvasObject -> Coord -> Coord -> IO ()

-- | Retrieve the position and (rectangular) size of the given Evas object
object_geometry_get :: EvasObject -> IO (Coord,Coord,Coord,Coord)
object_geometry_get obj = get4_helper (_object_geometry_get obj)

foreign import ccall "evas_object_geometry_get" _object_geometry_get :: EvasObject -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()



-- | Make the given Evas object visible
foreign import ccall "evas_object_show" object_show :: EvasObject -> IO ()

-- | Make the given Evas object invisible
foreign import ccall "evas_object_hide" object_hide :: EvasObject -> IO ()

-- | Retrieve whether or not the given Evas object is visible
object_visible_get :: EvasObject -> IO Bool
object_visible_get obj = toBool <$> object_visible_get_ obj

foreign import ccall "evas_object_visible_get" object_visible_get_ :: EvasObject -> IO EinaBool



-- | Set the general/main color of the given Evas object to the given one
foreign import ccall "evas_object_color_set" object_color_set :: EvasObject -> Int -> Int -> Int -> Int -> IO ()

-- | Set the general/main color of the given Evas object to the given one
object_color_get :: EvasObject -> IO (Int,Int,Int,Int)
object_color_get obj = get4_helper (_object_color_get obj)

foreign import ccall "evas_object_color_get" _object_color_get :: EvasObject -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()


-- | Retrieve the Evas canvas that the given object lives on
foreign import ccall "evas_object_evas_get" object_evas_get :: EvasObject -> IO Evas


-- | Retrieve the type of the given Evas object
object_type_get :: EvasObject -> IO String
object_type_get obj = peekCString =<< _object_type_get obj

foreign import ccall "evas_object_type_get" _object_type_get :: EvasObject -> IO CString



-- | Raise object to the top of its layer
foreign import ccall "evas_object_raise" object_raise :: EvasObject -> IO ()

-- | Lower object to the bottom of its layer
foreign import ccall "evas_object_lower" object_lower :: EvasObject -> IO ()

-- | Stack an object immediately above another object
foreign import ccall "evas_object_stack_above" object_stack_above :: EvasObject -> EvasObject -> IO ()

-- | Stack an object immediately below another object
foreign import ccall "evas_object_stack_below" object_stack_below :: EvasObject -> EvasObject -> IO ()

-- | Get the Evas object stacked right above an object
foreign import ccall "evas_object_above_get" object_above_get :: EvasObject -> IO EvasObject

-- | Get the Evas object stacked right below an object
foreign import ccall "evas_object_below_get" object_below_get :: EvasObject -> IO EvasObject





foreign import ccall "evas_object_rectangle_add" evas_object_rectangle_add :: Evas -> IO EvasObject
foreign import ccall "evas_object_image_add" evas_object_image_add :: Evas -> IO EvasObject

foreign import ccall "evas_object_pass_events_set" evas_object_pass_events_set :: EvasObject -> Bool -> IO ()

foreign import ccall "evas_object_image_file_set" evas_object_image_file_set :: EvasObject -> CString -> CString -> IO ()
foreign import ccall "evas_object_image_load_error_get" evas_object_image_load_error_get :: EvasObject -> IO Int
foreign import ccall "evas_object_image_fill_set" evas_object_image_fill_set :: EvasObject -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall "evas_object_image_size_get" evas_object_image_size_get_ :: EvasObject -> Ptr Int -> Ptr Int -> IO ()

evas_object_image_size_get :: EvasObject -> IO (Int,Int)
evas_object_image_size_get obj = get2_helper (evas_object_image_size_get_ obj)

foreign import ccall "evas_object_event_callback_add" evas_object_event_callback_add :: EvasObject -> Int -> EvasObjectEventCb -> Ptr () -> IO ()

foreign import ccall "wrapper" evas_object_event_wrap_callback :: (Ptr () -> Evas -> EvasObject -> Ptr () -> IO ()) -> IO EvasObjectEventCb



type Evas = Ptr ()
type EvasObjectEventCb = FunPtr (Ptr () -> Evas -> EvasObject -> Ptr () -> IO ())

{#enum _Evas_Callback_Type as EvasCallbackType {underscoreToCase} deriving (Eq,Show) #}

foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Evas -> Ptr Int -> Ptr Int -> IO ()

evas_output_size_get :: Evas -> IO (Int,Int)
evas_output_size_get ev = get2_helper (evas_output_size_get_ ev)


foreign import ccall "evas_event_callback_add" evas_event_callback_add :: Evas -> Int -> EvasObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" evas_load_error_str :: Int -> IO CString

keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
