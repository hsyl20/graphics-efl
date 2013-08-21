{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Events where

import Foreign.Ptr
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types

-- | Add (register) a callback function to a given Evas object event
addObjectEventCallback :: Object -> CallbackType -> ObjectEventCb -> Ptr () -> IO ()
addObjectEventCallback obj typ = _object_event_callback_add obj (fromEnum typ)

foreign import ccall "evas_object_event_callback_add" _object_event_callback_add :: Object -> Int -> ObjectEventCb -> Ptr () -> IO ()


-- | Add (register) a callback function to a given Evas object event with a non-default priority set
addEventCallbackWithPriority :: Object -> CallbackType -> CallbackPriority -> ObjectEventCb -> Ptr () -> IO ()
addEventCallbackWithPriority obj typ = _object_event_callback_priority_add obj (fromEnum typ)

foreign import ccall "evas_object_event_callback_priority_add" _object_event_callback_priority_add :: Object -> Int -> CallbackPriority -> ObjectEventCb -> Ptr () -> IO ()


-- | Delete a callback function from an object
removeEventCallback :: Object -> CallbackType -> ObjectEventCb -> IO (Ptr ())
removeEventCallback obj typ = _object_event_callback_del obj (fromEnum typ)

foreign import ccall "evas_object_event_callback_del" _object_event_callback_del :: Object -> Int -> ObjectEventCb -> IO (Ptr ())


-- | Delete (unregister) a callback function registered to a given Evas object event
removeEventCallbackFull :: Object -> CallbackType -> ObjectEventCb -> Ptr () -> IO (Ptr ())
removeEventCallbackFull obj typ = _object_event_callback_del_full obj (fromEnum typ)

foreign import ccall "evas_object_event_callback_del_full" _object_event_callback_del_full :: Object -> Int -> ObjectEventCb -> Ptr () -> IO (Ptr ())


-- | Set whether an Evas object is to pass (ignore) events
setPassEvents :: Object -> Bool -> IO ()
setPassEvents obj b = _object_pass_events_set obj (fromBool b)

enablePassEvents :: Object -> IO ()
enablePassEvents obj = setPassEvents obj True

disablePassEvents :: Object -> IO ()
disablePassEvents obj = setPassEvents obj False

foreign import ccall "evas_object_pass_events_set" _object_pass_events_set :: Object -> EinaBool -> IO ()


-- | Determine whether an object is set to pass (ignore) events
getPassEvents :: Object -> IO Bool
getPassEvents obj = toBool <$> _object_pass_events_get obj 

foreign import ccall "evas_object_pass_events_get" _object_pass_events_get :: Object -> IO EinaBool



-- | Set whether an Evas object is to repeat events
setRepeatEvents :: Object -> Bool -> IO ()
setRepeatEvents obj b = _object_repeat_events_set obj (fromBool b)

foreign import ccall "evas_object_repeat_events_set" _object_repeat_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to repeat events
getRepeatEvents :: Object -> IO Bool
getRepeatEvents obj = toBool <$> _object_repeat_events_get obj 

foreign import ccall "evas_object_repeat_events_get" _object_repeat_events_get :: Object -> IO EinaBool


-- | Set whether an Evas object is to propagate events
setPropagateEvents :: Object -> Bool -> IO ()
setPropagateEvents obj b = _object_propagate_events_set obj (fromBool b)

foreign import ccall "evas_object_propagate_events_set" _object_propagate_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to propagate events
getPropagateEvents :: Object -> IO Bool
getPropagateEvents obj = toBool <$> _object_propagate_events_get obj 

foreign import ccall "evas_object_propagate_events_get" _object_propagate_events_get :: Object -> IO EinaBool


-- | Set whether an Evas object is to freeze events
setFreezeEvents :: Object -> Bool -> IO ()
setFreezeEvents obj b = _object_freeze_events_set obj (fromBool b)

foreign import ccall "evas_object_freeze_events_set" _object_freeze_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to freeze events
getFreezeEvents :: Object -> IO Bool
getFreezeEvents obj = toBool <$> _object_freeze_events_get obj 

foreign import ccall "evas_object_freeze_events_get" _object_freeze_events_get :: Object -> IO EinaBool
