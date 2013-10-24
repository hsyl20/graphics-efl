{-# Language ForeignFunctionInterface #-}

-- | Event accessors
module Graphics.Efl.Canvas.KeyEvents where

import Graphics.Efl.Canvas.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Control.Monad

#include <Evas.h>

-------------------------------------
-- Key Down
-------------------------------------

-- | The name string of the key pressed
keyDownKeyName :: KeyDownEvent -> IO String
keyDownKeyName = peekCString <=< #{peek Evas_Event_Key_Down, keyname}

-- | Data
keyDownData :: KeyDownEvent -> IO (Ptr ())
keyDownData = #{peek Evas_Event_Key_Down, data}

-- | Modifier keys
keyDownModifierKeys :: KeyDownEvent -> IO ModifierKeys
keyDownModifierKeys = #{peek Evas_Event_Key_Down, modifiers}

-- | Lock keys
keyDownLockKeys :: KeyDownEvent -> IO LockKeys
keyDownLockKeys = #{peek Evas_Event_Key_Down, locks}

-- | The logical key (e.g. shift+1 == exclamation)
keyDownKey :: KeyDownEvent -> IO String
keyDownKey = peekCString <=< #{peek Evas_Event_Key_Down, key}

-- | A UTF8 string if this keystroke has produced a visible string to be ADDED
keyDownString :: KeyDownEvent -> IO String
keyDownString = peekCString <=< #{peek Evas_Event_Key_Down, string}

-- | A UTF8 string if this keystroke has modified a string in the middle of being composed - this string replaces the previous one
keyDownCompose :: KeyDownEvent -> IO String
keyDownCompose = peekCString <=< #{peek Evas_Event_Key_Down, compose}

-- | Timestamp
keyDownTimestamp :: KeyDownEvent -> IO CUInt
keyDownTimestamp = #{peek Evas_Event_Key_Down, timestamp}

-- | Event flags
keyDownFlags :: KeyDownEvent -> IO EventFlags
keyDownFlags = #{peek Evas_Event_Key_Down, event_flags}

-- | Device
keyDownDevice :: KeyDownEvent -> IO Device
keyDownDevice = #{peek Evas_Event_Key_Down, dev}

-------------------------------------
-- Key Up
-------------------------------------

-- | The name string of the key pressed
keyUpKeyName :: KeyUpEvent -> IO String
keyUpKeyName = peekCString <=< #{peek Evas_Event_Key_Up, keyname}

-- | Data
keyUpData :: KeyUpEvent -> IO (Ptr ())
keyUpData = #{peek Evas_Event_Key_Up, data}

-- | Modifier keys
keyUpModifierKeys :: KeyUpEvent -> IO ModifierKeys
keyUpModifierKeys = #{peek Evas_Event_Key_Up, modifiers}

-- | Lock keys
keyUpLockKeys :: KeyUpEvent -> IO LockKeys
keyUpLockKeys = #{peek Evas_Event_Key_Up, locks}

-- | The logical key (e.g. shift+1 == exclamation)
keyUpKey :: KeyUpEvent -> IO String
keyUpKey = peekCString <=< #{peek Evas_Event_Key_Up, key}

-- | A UTF8 string if this keystroke has produced a visible string to be ADDED
keyUpString :: KeyUpEvent -> IO String
keyUpString = peekCString <=< #{peek Evas_Event_Key_Up, string}

-- | A UTF8 string if this keystroke has modified a string in the middle of being composed - this string replaces the previous one
keyUpCompose :: KeyUpEvent -> IO String
keyUpCompose = peekCString <=< #{peek Evas_Event_Key_Up, compose}

-- | Timestamp
keyUpTimestamp :: KeyUpEvent -> IO CUInt
keyUpTimestamp = #{peek Evas_Event_Key_Up, timestamp}

-- | Event flags
keyUpFlags :: KeyUpEvent -> IO EventFlags
keyUpFlags = #{peek Evas_Event_Key_Up, event_flags}

-- | Device
keyUpDevice :: KeyUpEvent -> IO Device
keyUpDevice = #{peek Evas_Event_Key_Up, dev}

