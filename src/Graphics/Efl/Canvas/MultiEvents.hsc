{-# Language ForeignFunctionInterface #-}

-- | Event accessors
module Graphics.Efl.Canvas.MultiEvents where

import Graphics.Efl.Canvas.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Control.Applicative

#include <Evas.h>

-------------------------------------
-- Multi Down
-------------------------------------

-- | Multi device number that went down (1 or more for extra touches)
multiDownDeviceNumber :: MultiDownEvent -> IO Int
multiDownDeviceNumber ev = fromIntegral <$> (#{peek Evas_Event_Multi_Down, device} ev :: IO CInt)

-- | Radius
multiDownRadius :: MultiDownEvent -> IO Double
multiDownRadius = #{peek Evas_Event_Multi_Down, radius}

-- | Radius X
multiDownRadiusX :: MultiDownEvent -> IO Double
multiDownRadiusX = #{peek Evas_Event_Multi_Down, radius_x}

-- | Radius Y
multiDownRadiusY :: MultiDownEvent -> IO Double
multiDownRadiusY = #{peek Evas_Event_Multi_Down, radius_y}

-- | Pressure
multiDownPressure :: MultiDownEvent -> IO Double
multiDownPressure = #{peek Evas_Event_Multi_Down, pressure}

-- | Angle
multiDownAngle :: MultiDownEvent -> IO Double
multiDownAngle = #{peek Evas_Event_Multi_Down, angle}

-- | X/Y location of the cursor in world coordinates
multiDownWorldXY :: MultiDownEvent -> IO Point
multiDownWorldXY = #{peek Evas_Event_Multi_Down, output}

-- | X/Y location of the cursor in canvas coordinates
multiDownCanvasXY :: MultiDownEvent -> IO Point
multiDownCanvasXY = #{peek Evas_Event_Multi_Down, canvas}

-- | Data
multiDownData :: MultiDownEvent -> IO (Ptr ())
multiDownData = #{peek Evas_Event_Multi_Down, data}

-- | Modifier keys
multiDownModifierKeys :: MultiDownEvent -> IO ModifierKeys
multiDownModifierKeys = #{peek Evas_Event_Multi_Down, modifiers}

-- | Lock keys
multiDownLockKeys :: MultiDownEvent -> IO LockKeys
multiDownLockKeys = #{peek Evas_Event_Multi_Down, locks}

-- | Button flags
multiDownButtonFlags :: MultiDownEvent -> IO ButtonFlags
multiDownButtonFlags = #{peek Evas_Event_Multi_Down, flags}

-- | Timestamp
multiDownTimestamp :: MultiDownEvent -> IO CUInt
multiDownTimestamp = #{peek Evas_Event_Multi_Down, timestamp}

-- | Event flags
multiDownFlags :: MultiDownEvent -> IO EventFlags
multiDownFlags = #{peek Evas_Event_Multi_Down, event_flags}

-- | Device
multiDownDevice :: MultiDownEvent -> IO Device
multiDownDevice = #{peek Evas_Event_Multi_Down, dev}

-------------------------------------
-- Multi Up
-------------------------------------

-- | Multi device number that went down (1 or more for extra touches)
multiUpDeviceNumber :: MultiUpEvent -> IO Int
multiUpDeviceNumber ev = fromIntegral <$> (#{peek Evas_Event_Multi_Up, device} ev :: IO CInt)

-- | Radius
multiUpRadius :: MultiUpEvent -> IO Double
multiUpRadius = #{peek Evas_Event_Multi_Up, radius}

-- | Radius X
multiUpRadiusX :: MultiUpEvent -> IO Double
multiUpRadiusX = #{peek Evas_Event_Multi_Up, radius_x}

-- | Radius Y
multiUpRadiusY :: MultiUpEvent -> IO Double
multiUpRadiusY = #{peek Evas_Event_Multi_Up, radius_y}

-- | Pressure
multiUpPressure :: MultiUpEvent -> IO Double
multiUpPressure = #{peek Evas_Event_Multi_Up, pressure}

-- | Angle
multiUpAngle :: MultiUpEvent -> IO Double
multiUpAngle = #{peek Evas_Event_Multi_Up, angle}

-- | X/Y location of the cursor in world coordinates
multiUpWorldXY :: MultiUpEvent -> IO Point
multiUpWorldXY = #{peek Evas_Event_Multi_Up, output}

-- | X/Y location of the cursor in canvas coordinates
multiUpCanvasXY :: MultiUpEvent -> IO Point
multiUpCanvasXY = #{peek Evas_Event_Multi_Up, canvas}

-- | Data
multiUpData :: MultiUpEvent -> IO (Ptr ())
multiUpData = #{peek Evas_Event_Multi_Up, data}

-- | Modifier keys
multiUpModifierKeys :: MultiUpEvent -> IO ModifierKeys
multiUpModifierKeys = #{peek Evas_Event_Multi_Up, modifiers}

-- | Lock keys
multiUpLockKeys :: MultiUpEvent -> IO LockKeys
multiUpLockKeys = #{peek Evas_Event_Multi_Up, locks}

-- | Button flags
multiUpButtonFlags :: MultiUpEvent -> IO ButtonFlags
multiUpButtonFlags = #{peek Evas_Event_Multi_Up, flags}

-- | Timestamp
multiUpTimestamp :: MultiUpEvent -> IO CUInt
multiUpTimestamp = #{peek Evas_Event_Multi_Up, timestamp}

-- | Event flags
multiUpFlags :: MultiUpEvent -> IO EventFlags
multiUpFlags = #{peek Evas_Event_Multi_Up, event_flags}

-- | Device
multiUpDevice :: MultiUpEvent -> IO Device
multiUpDevice = #{peek Evas_Event_Multi_Up, dev}

-------------------------------------
-- Multi Move
-------------------------------------

-- | Multi device number that went down (1 or more for extra touches)
multiMoveDeviceNumber :: MultiMoveEvent -> IO Int
multiMoveDeviceNumber ev = fromIntegral <$> (#{peek Evas_Event_Multi_Move, device} ev :: IO CInt)

-- | Radius
multiMoveRadius :: MultiMoveEvent -> IO Double
multiMoveRadius = #{peek Evas_Event_Multi_Move, radius}

-- | Radius X
multiMoveRadiusX :: MultiMoveEvent -> IO Double
multiMoveRadiusX = #{peek Evas_Event_Multi_Move, radius_x}

-- | Radius Y
multiMoveRadiusY :: MultiMoveEvent -> IO Double
multiMoveRadiusY = #{peek Evas_Event_Multi_Move, radius_y}

-- | Pressure
multiMovePressure :: MultiMoveEvent -> IO Double
multiMovePressure = #{peek Evas_Event_Multi_Move, pressure}

-- | Angle
multiMoveAngle :: MultiMoveEvent -> IO Double
multiMoveAngle = #{peek Evas_Event_Multi_Move, angle}

-- | X/Y location of the cursor in world coordinates
multiMoveWorldXY :: MultiMoveEvent -> IO Point
multiMoveWorldXY p = #{peek Evas_Precision_Position, output} cur
   where cur = #{ptr Evas_Event_Multi_Move, cur} p

-- | X/Y location of the cursor in canvas coordinates
multiMoveCanvasXY :: MultiMoveEvent -> IO Point
multiMoveCanvasXY p = Point <$> x <*> y
   where 
      x = #{peek Evas_Coord_Precision_Point, x} canvas
      y = #{peek Evas_Coord_Precision_Point, y} canvas
      canvas = #{ptr Evas_Precision_Position, canvas} cur
      cur = #{ptr Evas_Event_Multi_Move, cur} p

-- | Precise X/Y location of the cursor in canvas coordinates
multiMoveCanvasXYsub :: MultiMoveEvent -> IO (Double,Double)
multiMoveCanvasXYsub p = (,) <$> x <*> y
   where 
      x = #{peek Evas_Coord_Precision_Point, xsub} canvas
      y = #{peek Evas_Coord_Precision_Point, ysub} canvas
      canvas = #{ptr Evas_Precision_Position, canvas} cur
      cur = #{ptr Evas_Event_Multi_Move, cur} p

-- | Data
multiMoveData :: MultiMoveEvent -> IO (Ptr ())
multiMoveData = #{peek Evas_Event_Multi_Move, data}

-- | Modifier keys
multiMoveModifierKeys :: MultiMoveEvent -> IO ModifierKeys
multiMoveModifierKeys = #{peek Evas_Event_Multi_Move, modifiers}

-- | Lock keys
multiMoveLockKeys :: MultiMoveEvent -> IO LockKeys
multiMoveLockKeys = #{peek Evas_Event_Multi_Move, locks}

-- | Timestamp
multiMoveTimestamp :: MultiMoveEvent -> IO CUInt
multiMoveTimestamp = #{peek Evas_Event_Multi_Move, timestamp}

-- | Event flags
multiMoveFlags :: MultiMoveEvent -> IO EventFlags
multiMoveFlags = #{peek Evas_Event_Multi_Move, event_flags}

-- | Device
multiMoveDevice :: MultiMoveEvent -> IO Device
multiMoveDevice = #{peek Evas_Event_Multi_Move, dev}

