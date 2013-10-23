{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas (
   module M,
   initCanvas, shutdownCanvas,
   createCanvas, destroyCanvas,
   focusCanvasIn, focusCanvasOut, isCanvasFocused,
   increaseNoChangeCounter, decreaseNoChangeCounter,
   attachCanvasData, getCanvasAttachedData,
   addDamageRectangle, addObscuredRectangle, clearObscuredRegions,
   renderCanvas, norenderCanvas, discardCanvasRenderingCache, discardCanvasData
) where

import Graphics.Efl.Canvas.Types as M
import Graphics.Efl.Canvas.BasicObject as M
import Graphics.Efl.Canvas.Rectangle as M
import Graphics.Efl.Canvas.Line as M
import Graphics.Efl.Canvas.Polygon as M
import Graphics.Efl.Canvas.Image as M
import Graphics.Efl.Canvas.Events as M
import Graphics.Efl.Canvas.MouseEvents as M
import Graphics.Efl.Canvas.Misc as M
import Graphics.Efl.Canvas.Text as M
import Graphics.Efl.Canvas.TextBlock as M
import Graphics.Efl.Canvas.Map as M
import Graphics.Efl.Canvas.Engine as M
import Graphics.Efl.Canvas.ViewPort as M
import Graphics.Efl.Canvas.Pointer as M

import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative
import Graphics.Efl.Eina

-- | Initialize the canvas manager. Consider using @getWindowCanvas@ instead
foreign import ccall "evas_init" initCanvas :: IO CInt

-- | Shutdown the canvas manager.
foreign import ccall "evas_shutdown" shutdownCanvas :: IO ()

-- | Create a canvas
foreign import ccall "evas_new" createCanvas :: IO Canvas

-- | Destroy a canvas
foreign import ccall "evas_free" destroyCanvas :: Canvas -> IO ()

-- | Inform the canvas that it got the focus
foreign import ccall "evas_focus_in" focusCanvasIn :: Canvas -> IO ()

-- | Inform the canvas that it lost the focus
foreign import ccall "evas_focus_out" focusCanvasOut :: Canvas -> IO ()

-- | Get the focus state known by the cancas
isCanvasFocused :: Canvas -> IO Bool
isCanvasFocused canvas = toBool <$> _evas_focus_state_get canvas

foreign import ccall "evas_focus_state_get" _evas_focus_state_get :: Canvas -> IO EinaBool

-- | Increase the no-change counter
--
-- This tells the canvas that while the no-change counter is greater than 0, do not
-- mark objects as "changed" when making changes.
foreign import ccall "evas_nochange_push" increaseNoChangeCounter :: Canvas -> IO ()

-- | Decrease the no-change counter
foreign import ccall "evas_nochange_pop" decreaseNoChangeCounter :: Canvas -> IO ()

-- | Attach data to the canvas
foreign import ccall "evas_data_attach_set" attachCanvasData :: Canvas -> Ptr () -> IO ()

-- | Get data attached to the canvas
foreign import ccall "evas_data_attach_get" getCanvasAttachedData :: Canvas -> IO (Ptr ())

-- | Add a damage rectangle
foreign import ccall "evas_damage_rectangle_add" addDamageRectangle :: Canvas -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Add an obscured rectangle
foreign import ccall "evas_obscured_rectangle_add" addObscuredRectangle :: Canvas -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Clear obscured regions
foreign import ccall "evas_obscured_clear" clearObscuredRegions :: Canvas -> IO ()

-- | Force renderization of the canvas
foreign import ccall "evas_render" renderCanvas :: Canvas -> IO ()

-- | Update the canvas internal objects but not triggering immediate renderization.
foreign import ccall "evas_norender" norenderCanvas :: Canvas -> IO ()

-- | Discard cached data used for the rendering
foreign import ccall "evas_render_idle_flush" discardCanvasRenderingCache :: Canvas -> IO ()

-- | Make the canvas discard as much data as possible used by the engine at runtime.
foreign import ccall "evas_render_dump" discardCanvasData :: Canvas -> IO ()
