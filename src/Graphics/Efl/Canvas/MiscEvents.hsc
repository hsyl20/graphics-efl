{-# Language ForeignFunctionInterface #-}

-- | Event accessors
module Graphics.Efl.Canvas.MiscEvents where

import Graphics.Efl.Canvas.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

#include <Evas.h>

-------------------------------------
-- Hold event
-------------------------------------

-- | The name string of the key pressed
holdFlag :: HoldEvent -> IO Int
holdFlag ev = fromIntegral <$> (#{peek Evas_Event_Hold, hold} ev :: IO CInt)

-- | Data
holdData :: HoldEvent -> IO (Ptr ())
holdData = #{peek Evas_Event_Hold, data}

-- | Timestamp
holdTimestamp :: HoldEvent -> IO CUInt
holdTimestamp = #{peek Evas_Event_Hold, timestamp}

-- | Event flags
holdFlags :: HoldEvent -> IO EventFlags
holdFlags = #{peek Evas_Event_Hold, event_flags}

-- | Device
holdDevice :: HoldEvent -> IO Device
holdDevice = #{peek Evas_Event_Hold, dev}
