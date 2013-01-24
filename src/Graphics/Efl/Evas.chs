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



foreign import ccall "evas_object_rectangle_add" evas_object_rectangle_add :: Evas -> IO EvasObject
foreign import ccall "evas_object_image_add" evas_object_image_add :: Evas -> IO EvasObject

foreign import ccall "evas_object_color_set" evas_object_color_set :: EvasObject -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "evas_object_resize" evas_object_resize :: EvasObject -> Int -> Int -> IO ()
foreign import ccall "evas_object_move" evas_object_move :: EvasObject -> Int -> Int -> IO ()
foreign import ccall "evas_object_show" evas_object_show :: EvasObject -> IO ()
foreign import ccall "evas_object_focus_set" evas_object_focus_set :: EvasObject -> Bool -> IO ()

foreign import ccall "evas_object_geometry_get" evas_object_geometry_get_ :: EvasObject -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int-> IO ()

evas_object_geometry_get :: EvasObject -> IO (Int,Int,Int,Int)
evas_object_geometry_get obj = geometry_get_helper (evas_object_geometry_get_ obj)

foreign import ccall "evas_object_pass_events_set" evas_object_pass_events_set :: EvasObject -> Bool -> IO ()

foreign import ccall "evas_object_image_file_set" evas_object_image_file_set :: EvasObject -> CString -> CString -> IO ()
foreign import ccall "evas_object_image_load_error_get" evas_object_image_load_error_get :: EvasObject -> IO Int
foreign import ccall "evas_object_image_fill_set" evas_object_image_fill_set :: EvasObject -> Int -> Int -> Int -> Int -> IO ()

foreign import ccall "evas_object_image_size_get" evas_object_image_size_get_ :: EvasObject -> Ptr Int -> Ptr Int -> IO ()

evas_object_image_size_get :: EvasObject -> IO (Int,Int)
evas_object_image_size_get obj = size_get_helper (evas_object_image_size_get_ obj)

foreign import ccall "evas_object_event_callback_add" evas_object_event_callback_add :: EvasObject -> Int -> EvasObjectEventCb -> Ptr () -> IO ()

foreign import ccall "wrapper" evas_object_event_wrap_callback :: (Ptr () -> Evas -> EvasObject -> Ptr () -> IO ()) -> IO EvasObjectEventCb



type Evas = Ptr ()
type EvasObjectEventCb = FunPtr (Ptr () -> Evas -> EvasObject -> Ptr () -> IO ())

{#enum _Evas_Callback_Type as EvasCallbackType {underscoreToCase} deriving (Eq,Show) #}

foreign import ccall "evas_output_size_get" evas_output_size_get_ :: Evas -> Ptr Int -> Ptr Int -> IO ()

evas_output_size_get :: Evas -> IO (Int,Int)
evas_output_size_get ev = size_get_helper (evas_output_size_get_ ev)


foreign import ccall "evas_event_callback_add" evas_event_callback_add :: Evas -> Int -> EvasObjectEventCb -> Ptr () -> IO ()

foreign import ccall "evas_load_error_str" evas_load_error_str :: Int -> IO CString

keyDownKeyname :: Ptr () -> IO String
keyDownKeyname s = {# get Evas_Event_Key_Down->keyname #} s >>= peekCString

keyDownKey :: Ptr () -> IO String
keyDownKey s = {# get Evas_Event_Key_Down->key #} s >>= peekCString
