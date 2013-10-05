{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Events (
   addEventCallback, addEventCallbackWithPriority,
   removeEventCallback, removeEventCallbackFull,
   enableEventPassing, disableEventPassing, isPassingEvents,
   enableEventRepeating, disableEventRepeating, isRepeatingEvents,
   enableEventPropagating, disableEventPropagating, isPropagatingEvents,
   enableEventFreezing, disableEventFreezing, isFreezingEvents,
   setDefaultEventFlags, getDefaultEventFlags,
   freezeEvents, unfreezeEvents, getEventFreezeCount, updateAfterEventUnfreezing,
   getEventDownCount 
) where

import Foreign.Ptr
import Foreign.C.Types

import Control.Applicative

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types

-- | Add (register) a callback function to a given Evas object event
addEventCallback :: Object -> CallbackType -> ObjectEventCb -> Ptr () -> IO ()
addEventCallback obj typ = _object_event_callback_add obj (fromEnum typ)

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

-- | Enable event passing
enableEventPassing :: Object -> IO ()
enableEventPassing obj = setPassEvents obj True

-- | Disable event passing
disableEventPassing :: Object -> IO ()
disableEventPassing obj = setPassEvents obj False

foreign import ccall "evas_object_pass_events_set" _object_pass_events_set :: Object -> EinaBool -> IO ()


-- | Determine whether an object is set to pass (ignore) events
isPassingEvents :: Object -> IO Bool
isPassingEvents obj = toBool <$> _object_pass_events_get obj 

foreign import ccall "evas_object_pass_events_get" _object_pass_events_get :: Object -> IO EinaBool

-- | Set whether an Evas object is to repeat events
setRepeatEvents :: Object -> Bool -> IO ()
setRepeatEvents obj b = _object_repeat_events_set obj (fromBool b)

-- | Enable event repeating
enableEventRepeating :: Object -> IO ()
enableEventRepeating obj = setRepeatEvents obj True

-- | Disable event repeating
disableEventRepeating :: Object -> IO ()
disableEventRepeating obj = setRepeatEvents obj False

foreign import ccall "evas_object_repeat_events_set" _object_repeat_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to repeat events
isRepeatingEvents :: Object -> IO Bool
isRepeatingEvents obj = toBool <$> _object_repeat_events_get obj 

foreign import ccall "evas_object_repeat_events_get" _object_repeat_events_get :: Object -> IO EinaBool


-- | Set whether an Evas object is to propagate events
setPropagateEvents :: Object -> Bool -> IO ()
setPropagateEvents obj b = _object_propagate_events_set obj (fromBool b)

-- | Enable event propagating
enableEventPropagating :: Object -> IO ()
enableEventPropagating obj = setPropagateEvents obj True

-- | Disable event propagating
disableEventPropagating :: Object -> IO ()
disableEventPropagating obj = setPropagateEvents obj False

foreign import ccall "evas_object_propagate_events_set" _object_propagate_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to propagate events
isPropagatingEvents :: Object -> IO Bool
isPropagatingEvents obj = toBool <$> _object_propagate_events_get obj 

foreign import ccall "evas_object_propagate_events_get" _object_propagate_events_get :: Object -> IO EinaBool


-- | Set whether an Evas object is to freeze events
setFreezeEvents :: Object -> Bool -> IO ()
setFreezeEvents obj b = _object_freeze_events_set obj (fromBool b)

-- | Enable event freezing
enableEventFreezing :: Object -> IO ()
enableEventFreezing obj = setFreezeEvents obj True

-- | Disable event freezing
disableEventFreezing :: Object -> IO ()
disableEventFreezing obj = setFreezeEvents obj False

foreign import ccall "evas_object_freeze_events_set" _object_freeze_events_set :: Object -> EinaBool -> IO ()

-- | Determine whether an object is set to freeze events
isFreezingEvents :: Object -> IO Bool
isFreezingEvents obj = toBool <$> _object_freeze_events_get obj 

foreign import ccall "evas_object_freeze_events_get" _object_freeze_events_get :: Object -> IO EinaBool



-- | Set the default set of flags an event begins with
setDefaultEventFlags :: Canvas -> EventFlags -> IO ()
setDefaultEventFlags canvas flags = _setDefaultEventFlags canvas (fromEnum flags)


foreign import ccall "evas_event_default_flags_set" _setDefaultEventFlags :: Canvas -> Int -> IO ()

-- | Get the defaulty set of flags an event begins with
getDefaultEventFlags :: Canvas -> IO EventFlags
getDefaultEventFlags canvas = toEnum <$> _getDefaultEventFlags canvas

foreign import ccall "evas_event_default_flags_get" _getDefaultEventFlags :: Canvas -> IO Int


-- | Freeze all input events processing.
foreign import ccall "evas_event_freeze" freezeEvents :: Canvas -> IO ()

-- | Thaw a canvas out after freezing (for input events)
foreign import ccall "evas_event_thaw" unfreezeEvents :: Canvas -> IO ()

-- | Return the freeze count on input events of a given canvas
foreign import ccall "evas_event_freeze_get" getEventFreezeCount :: Canvas -> IO Int

-- | After thaw of a canvas, re-evaluate the state of objects and call callbacks
foreign import ccall "evas_event_thaw_eval" updateAfterEventUnfreezing :: Canvas -> IO ()

-- | Get the number of mouse or multi presses currently active
foreign import ccall "evas_event_down_count_get" getEventDownCount :: Canvas -> IO Int
