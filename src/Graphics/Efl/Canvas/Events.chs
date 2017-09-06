{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Events (
   addEventCallback, addEventCallbackWithPriority,
   removeEventCallback, removeEventCallbackFull,
   enableEventPassing, disableEventPassing,
   setObjectPassEvents, getObjectPassEvents,
   enableEventRepeating, disableEventRepeating, isRepeatingEvents,
   enableEventPropagating, disableEventPropagating, isPropagatingEvents,
   enableEventFreezing, disableEventFreezing, isFreezingEvents,
   setDefaultEventFlags, getDefaultEventFlags,
   freezeEvents, unfreezeEvents, getEventFreezeCount, updateAfterEventUnfreezing,
   getEventDownCount,
   onEvent, onMouseDown, onMouseUp, onMouseIn, onMouseOut,
   onMouseMove, onMouseWheel,
   onMultiDown, onMultiUp, onMultiMove,
   onKeyDown, onKeyUp, onFree, onHold,
   onObjectFocusIn, onObjectFocusOut,
   onObjectShow, onObjectHide, onObjectMove, onObjectResize
) where

import Foreign.Ptr
import Foreign.C.Types

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types

#include <Evas.h>

-- | Key Down event setter
onKeyDown :: (Object -> KeyDownEvent -> IO ()) -> Object -> IO ()
onKeyDown = onEvent EvasCallbackKeyDown

-- | Key Up event setter
onKeyUp :: (Object -> KeyUpEvent -> IO ()) -> Object -> IO ()
onKeyUp = onEvent EvasCallbackKeyUp

-- | Free change event
onFree :: IO () -> Object -> IO ()
onFree f = onEvent EvasCallbackFree (\_ _ -> f)

-- | Hold change event
onHold :: (Object -> HoldEvent -> IO ()) -> Object -> IO ()
onHold = onEvent EvasCallbackHold

-- | Multi Down event setter
onMultiDown :: (Object -> MultiDownEvent -> IO ()) -> Object -> IO ()
onMultiDown = onEvent EvasCallbackMultiDown

-- | Multi Up event setter
onMultiUp :: (Object -> MultiUpEvent -> IO ()) -> Object -> IO ()
onMultiUp = onEvent EvasCallbackMultiUp

-- | Multi Move event setter
onMultiMove :: (Object -> MultiMoveEvent -> IO ()) -> Object -> IO ()
onMultiMove = onEvent EvasCallbackMultiMove

-- | Mouse Down event setter
onMouseDown :: (Object -> MouseDownEvent -> IO ()) -> Object -> IO ()
onMouseDown = onEvent EvasCallbackMouseDown

-- | Mouse Up event setter
onMouseUp :: (Object -> MouseUpEvent -> IO ()) -> Object -> IO ()
onMouseUp = onEvent EvasCallbackMouseUp

-- | Mouse In event setter
onMouseIn :: (Object -> MouseInEvent -> IO ()) -> Object -> IO ()
onMouseIn = onEvent EvasCallbackMouseIn

-- | Mouse Out event setter
onMouseOut :: (Object -> MouseOutEvent -> IO ()) -> Object -> IO ()
onMouseOut = onEvent EvasCallbackMouseOut

-- | Mouse Move event setter
onMouseMove :: (Object -> MouseMoveEvent -> IO ()) -> Object -> IO ()
onMouseMove = onEvent EvasCallbackMouseMove

-- | Mouse Wheel event setter
onMouseWheel :: (Object -> MouseWheelEvent -> IO ()) -> Object -> IO ()
onMouseWheel = onEvent EvasCallbackMouseWheel

-- | Focus In
onObjectFocusIn :: IO () -> Object -> IO ()
onObjectFocusIn f = onEvent EvasCallbackFocusIn (\_ _ -> f)

-- | Focus Out
onObjectFocusOut :: IO () -> Object -> IO ()
onObjectFocusOut f = onEvent EvasCallbackFocusOut (\_ _ -> f)

-- | Show
onObjectShow :: Object -> IO () -> IO ()
onObjectShow o f = onEvent EvasCallbackShow (\_ _ -> f) o

-- | Hide
onObjectHide :: Object -> IO () -> IO ()
onObjectHide o f = onEvent EvasCallbackHide (\_ _ -> f) o

-- | Move
onObjectMove :: Object -> IO () -> IO ()
onObjectMove o f = onEvent EvasCallbackMove (\_ _ -> f) o

-- | Resize
onObjectResize :: Object -> IO () -> IO ()
onObjectResize o f = onEvent EvasCallbackResize (\_ _ -> f) o

-- | Generic event setter
onEvent :: CallbackType -> (Object -> Ptr a -> IO ()) -> Object -> IO ()
onEvent typ cb obj = flip (addEventCallback obj typ) nullPtr =<< wrapEventCallback (\_ _ o i -> cb o (castPtr i))

-- | Add (register) a callback function to a given Evas object event
addEventCallback :: Object -> CallbackType -> ObjectEventCb -> Ptr () -> IO ()
addEventCallback obj typ = _object_event_callback_add obj (fromIntegral $ fromEnum typ)

foreign import ccall "evas_object_event_callback_add" _object_event_callback_add :: Object -> CInt -> ObjectEventCb -> Ptr () -> IO ()


-- | Add (register) a callback function to a given Evas object event with a non-default priority set
addEventCallbackWithPriority :: Object -> CallbackType -> CallbackPriority -> ObjectEventCb -> Ptr () -> IO ()
addEventCallbackWithPriority obj typ = _object_event_callback_priority_add obj (fromIntegral $ fromEnum typ)

foreign import ccall "evas_object_event_callback_priority_add" _object_event_callback_priority_add :: Object -> CInt -> CallbackPriority -> ObjectEventCb -> Ptr () -> IO ()


-- | Delete a callback function from an object
removeEventCallback :: Object -> CallbackType -> ObjectEventCb -> IO (Ptr ())
removeEventCallback obj typ = _object_event_callback_del obj (fromIntegral $ fromEnum typ)

foreign import ccall "evas_object_event_callback_del" _object_event_callback_del :: Object -> CInt -> ObjectEventCb -> IO (Ptr ())


-- | Delete (unregister) a callback function registered to a given Evas object event
removeEventCallbackFull :: Object -> CallbackType -> ObjectEventCb -> Ptr () -> IO (Ptr ())
removeEventCallbackFull obj typ = _object_event_callback_del_full obj (fromIntegral $ fromEnum typ)

foreign import ccall "evas_object_event_callback_del_full" _object_event_callback_del_full :: Object -> CInt -> ObjectEventCb -> Ptr () -> IO (Ptr ())


-- | Set whether an Evas object is to pass (ignore) events
setObjectPassEvents :: Bool -> Object -> IO ()
setObjectPassEvents b obj = _object_pass_events_set obj (fromBool b)

-- | Enable event passing
enableEventPassing :: Object -> IO ()
enableEventPassing = setObjectPassEvents True

-- | Disable event passing
disableEventPassing :: Object -> IO ()
disableEventPassing = setObjectPassEvents False

foreign import ccall "evas_object_pass_events_set" _object_pass_events_set :: Object -> EinaBool -> IO ()


-- | Determine whether an object is set to pass (ignore) events
getObjectPassEvents :: Object -> IO Bool
getObjectPassEvents obj = toBool <$> _object_pass_events_get obj

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
setDefaultEventFlags canvas flags = _setDefaultEventFlags canvas (fromIntegral $ fromEnum flags)


foreign import ccall "evas_event_default_flags_set" _setDefaultEventFlags :: Canvas -> CInt -> IO ()

-- | Get the defaulty set of flags an event begins with
getDefaultEventFlags :: Canvas -> IO EventFlags
getDefaultEventFlags canvas = toEnum . fromIntegral <$> _getDefaultEventFlags canvas

foreign import ccall "evas_event_default_flags_get" _getDefaultEventFlags :: Canvas -> IO CInt


-- | Freeze all input events processing.
foreign import ccall "evas_event_freeze" freezeEvents :: Canvas -> IO ()

-- | Thaw a canvas out after freezing (for input events)
foreign import ccall "evas_event_thaw" unfreezeEvents :: Canvas -> IO ()

-- | Return the freeze count on input events of a given canvas
foreign import ccall "evas_event_freeze_get" getEventFreezeCount :: Canvas -> IO CInt

-- | After thaw of a canvas, re-evaluate the state of objects and call callbacks
foreign import ccall "evas_event_thaw_eval" updateAfterEventUnfreezing :: Canvas -> IO ()

-- | Get the number of mouse or multi presses currently active
foreign import ccall "evas_event_down_count_get" getEventDownCount :: Canvas -> IO CInt
