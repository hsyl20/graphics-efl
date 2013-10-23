{-# Language ForeignFunctionInterface #-}

-- | Event accessors
module Graphics.Efl.Canvas.MouseEvents where

import Graphics.Efl.Canvas.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Control.Applicative

#include <Evas.h>

-------------------------------------
-- Mouse Down
-------------------------------------

-- | Button pressed mask
mouseDownButton :: MouseDownEvent -> IO Int
mouseDownButton ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Down, button} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseDownWorldXY :: MouseDownEvent -> IO Point
mouseDownWorldXY = #{peek Evas_Event_Mouse_Down, output}

-- | X/Y location of the cursor in canvas coordinates
mouseDownCanvasXY :: MouseDownEvent -> IO Point
mouseDownCanvasXY = #{peek Evas_Event_Mouse_Down, canvas}

-- | Data
mouseDownData :: MouseDownEvent -> IO (Ptr ())
mouseDownData = #{peek Evas_Event_Mouse_Down, data}

-- | Modifier keys
mouseDownModifierKeys :: MouseDownEvent -> IO ModifierKeys
mouseDownModifierKeys = #{peek Evas_Event_Mouse_Down, modifiers}

-- | Lock keys
mouseDownLockKeys :: MouseDownEvent -> IO LockKeys
mouseDownLockKeys = #{peek Evas_Event_Mouse_Down, locks}

-- | Timestamp
mouseDownTimestamp :: MouseDownEvent -> IO CUInt
mouseDownTimestamp = #{peek Evas_Event_Mouse_Down, timestamp}

-- | Button flags
mouseDownButtonFlags :: MouseDownEvent -> IO ButtonFlags
mouseDownButtonFlags = #{peek Evas_Event_Mouse_Down, flags}

-- | Event flags
mouseDownFlags :: MouseDownEvent -> IO EventFlags
mouseDownFlags = #{peek Evas_Event_Mouse_Down, event_flags}

-- | Device
mouseDownDevice :: MouseDownEvent -> IO Device
mouseDownDevice = #{peek Evas_Event_Mouse_Down, dev}

-------------------------------------
-- Mouse Up
-------------------------------------

-- | Button pressed mask
mouseUpButton :: MouseUpEvent -> IO Int
mouseUpButton ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Up, button} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseUpWorldXY :: MouseUpEvent -> IO Point
mouseUpWorldXY = #{peek Evas_Event_Mouse_Up, output}

-- | X/Y location of the cursor in canvas coordinates
mouseUpCanvasXY :: MouseUpEvent -> IO Point
mouseUpCanvasXY = #{peek Evas_Event_Mouse_Up, canvas}

-- | Data
mouseUpData :: MouseUpEvent -> IO (Ptr ())
mouseUpData = #{peek Evas_Event_Mouse_Up, data}

-- | Modifier keys
mouseUpModifierKeys :: MouseUpEvent -> IO ModifierKeys
mouseUpModifierKeys = #{peek Evas_Event_Mouse_Up, modifiers}

-- | Lock keys
mouseUpLockKeys :: MouseUpEvent -> IO LockKeys
mouseUpLockKeys = #{peek Evas_Event_Mouse_Up, locks}

-- | Timestamp
mouseUpTimestamp :: MouseUpEvent -> IO CUInt
mouseUpTimestamp = #{peek Evas_Event_Mouse_Up, timestamp}

-- | Button flags
mouseUpButtonFlags :: MouseUpEvent -> IO ButtonFlags
mouseUpButtonFlags = #{peek Evas_Event_Mouse_Up, flags}

-- | Event flags
mouseUpFlags :: MouseUpEvent -> IO EventFlags
mouseUpFlags = #{peek Evas_Event_Mouse_Up, event_flags}

-- | Device
mouseUpDevice :: MouseUpEvent -> IO Device
mouseUpDevice = #{peek Evas_Event_Mouse_Up, dev}

-------------------------------------
-- Mouse In
-------------------------------------

-- | Button pressed mask
mouseInButtons :: MouseInEvent -> IO Int
mouseInButtons ev = fromIntegral <$> (#{peek Evas_Event_Mouse_In, buttons} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseInWorldXY :: MouseInEvent -> IO Point
mouseInWorldXY = #{peek Evas_Event_Mouse_In, output}

-- | X/Y location of the cursor in canvas coordinates
mouseInCanvasXY :: MouseInEvent -> IO Point
mouseInCanvasXY = #{peek Evas_Event_Mouse_In, canvas}

-- | Data
mouseInData :: MouseInEvent -> IO (Ptr ())
mouseInData = #{peek Evas_Event_Mouse_In, data}

-- | Modifier keys
mouseInModifierKeys :: MouseInEvent -> IO ModifierKeys
mouseInModifierKeys = #{peek Evas_Event_Mouse_In, modifiers}

-- | Lock keys
mouseInLockKeys :: MouseInEvent -> IO LockKeys
mouseInLockKeys = #{peek Evas_Event_Mouse_In, locks}

-- | Timestamp
mouseInTimestamp :: MouseInEvent -> IO CUInt
mouseInTimestamp = #{peek Evas_Event_Mouse_In, timestamp}

-- | Event flags
mouseInFlags :: MouseInEvent -> IO EventFlags
mouseInFlags = #{peek Evas_Event_Mouse_In, event_flags}

-- | Device
mouseInDevice :: MouseInEvent -> IO Device
mouseInDevice = #{peek Evas_Event_Mouse_In, dev}

-------------------------------------
-- Mouse Out
-------------------------------------

-- | Button pressed mask
mouseOutButtons :: MouseOutEvent -> IO Int
mouseOutButtons ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Out, buttons} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseOutWorldXY :: MouseOutEvent -> IO Point
mouseOutWorldXY = #{peek Evas_Event_Mouse_Out, output}

-- | X/Y location of the cursor in canvas coordinates
mouseOutCanvasXY :: MouseOutEvent -> IO Point
mouseOutCanvasXY = #{peek Evas_Event_Mouse_Out, canvas}

-- | Data
mouseOutData :: MouseOutEvent -> IO (Ptr ())
mouseOutData = #{peek Evas_Event_Mouse_Out, data}

-- | Modifier keys
mouseOutModifierKeys :: MouseOutEvent -> IO ModifierKeys
mouseOutModifierKeys = #{peek Evas_Event_Mouse_Out, modifiers}

-- | Lock keys
mouseOutLockKeys :: MouseOutEvent -> IO LockKeys
mouseOutLockKeys = #{peek Evas_Event_Mouse_Out, locks}

-- | Timestamp
mouseOutTimestamp :: MouseOutEvent -> IO CUInt
mouseOutTimestamp = #{peek Evas_Event_Mouse_Out, timestamp}

-- | Event flags
mouseOutFlags :: MouseOutEvent -> IO EventFlags
mouseOutFlags = #{peek Evas_Event_Mouse_Out, event_flags}

-- | Device
mouseOutDevice :: MouseOutEvent -> IO Device
mouseOutDevice = #{peek Evas_Event_Mouse_Out, dev}

-------------------------------------
-- Mouse Move
-------------------------------------

-- | Button pressed mask
mouseMoveButtons :: MouseMoveEvent -> IO Int
mouseMoveButtons ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Move, buttons} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseMoveWorldXY :: MouseMoveEvent -> IO Point
mouseMoveWorldXY p = #{peek Evas_Position, output} cur
   where cur = #{ptr Evas_Event_Mouse_Move, cur} p
   

-- | X/Y location of the cursor in canvas coordinates
mouseMoveCanvasXY :: MouseMoveEvent -> IO Point
mouseMoveCanvasXY p = #{peek Evas_Position, canvas} cur
   where cur = #{ptr Evas_Event_Mouse_Move, cur} p

-- | X/Y location of the prevsor in world coordinates
mouseMovePreviousWorldXY :: MouseMoveEvent -> IO Point
mouseMovePreviousWorldXY p = #{peek Evas_Position, output} prev
   where prev = #{ptr Evas_Event_Mouse_Move, prev} p
   

-- | X/Y location of the prevsor in canvas coordinates
mouseMovePreviousCanvasXY :: MouseMoveEvent -> IO Point
mouseMovePreviousCanvasXY p = #{peek Evas_Position, canvas} prev
   where prev = #{ptr Evas_Event_Mouse_Move, prev} p
   

-- | Data
mouseMoveData :: MouseMoveEvent -> IO (Ptr ())
mouseMoveData = #{peek Evas_Event_Mouse_Move, data}

-- | Modifier keys
mouseMoveModifierKeys :: MouseMoveEvent -> IO ModifierKeys
mouseMoveModifierKeys = #{peek Evas_Event_Mouse_Move, modifiers}

-- | Lock keys
mouseMoveLockKeys :: MouseMoveEvent -> IO LockKeys
mouseMoveLockKeys = #{peek Evas_Event_Mouse_Move, locks}

-- | Timestamp
mouseMoveTimestamp :: MouseMoveEvent -> IO CUInt
mouseMoveTimestamp = #{peek Evas_Event_Mouse_Move, timestamp}

-- | Event flags
mouseMoveFlags :: MouseMoveEvent -> IO EventFlags
mouseMoveFlags = #{peek Evas_Event_Mouse_Move, event_flags}

-- | Device
mouseMoveDevice :: MouseMoveEvent -> IO Device
mouseMoveDevice = #{peek Evas_Event_Mouse_Move, dev}

-------------------------------------
-- Mouse Wheel
-------------------------------------

-- | Wheel direction
mouseWheelDirection :: MouseWheelEvent -> IO Int
mouseWheelDirection ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Wheel, direction} ev :: IO CInt)

-- | Wheel offset
mouseWheelOffset :: MouseWheelEvent -> IO Int
mouseWheelOffset ev = fromIntegral <$> (#{peek Evas_Event_Mouse_Wheel, z} ev :: IO CInt)

-- | X/Y location of the cursor in world coordinates
mouseWheelWorldXY :: MouseWheelEvent -> IO Point
mouseWheelWorldXY = #{peek Evas_Event_Mouse_Wheel, output}

-- | X/Y location of the cursor in canvas coordinates
mouseWheelCanvasXY :: MouseWheelEvent -> IO Point
mouseWheelCanvasXY = #{peek Evas_Event_Mouse_Wheel, canvas}

-- | Data
mouseWheelData :: MouseWheelEvent -> IO (Ptr ())
mouseWheelData = #{peek Evas_Event_Mouse_Wheel, data}

-- | Modifier keys
mouseWheelModifierKeys :: MouseWheelEvent -> IO ModifierKeys
mouseWheelModifierKeys = #{peek Evas_Event_Mouse_Wheel, modifiers}

-- | Lock keys
mouseWheelLockKeys :: MouseWheelEvent -> IO LockKeys
mouseWheelLockKeys = #{peek Evas_Event_Mouse_Wheel, locks}

-- | Timestamp
mouseWheelTimestamp :: MouseWheelEvent -> IO CUInt
mouseWheelTimestamp = #{peek Evas_Event_Mouse_Wheel, timestamp}

-- | Event flags
mouseWheelFlags :: MouseWheelEvent -> IO EventFlags
mouseWheelFlags = #{peek Evas_Event_Mouse_Wheel, event_flags}

-- | Device
mouseWheelDevice :: MouseWheelEvent -> IO Device
mouseWheelDevice = #{peek Evas_Event_Mouse_Wheel, dev}

