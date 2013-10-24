{-# Language ForeignFunctionInterface #-}

-- | EFL types
module Graphics.Efl.Canvas.Types (
   TextStyle(..), TextShadowStyle(..),
   BorderFillMode(..), FillSpread(..), LoadError(..),
   ColorSpace(..), ImageScaleHint(..), ImageContentHint(..),
   ImageAnimatedLoopType(..), TextDirection(..),
   Canvas, Object, Coord, ModifierKeys, LockKeys, Device,
   PixelImportSource, NativeSurface, VideoSurface,
   FontSize, CallbackPriority, Map, EngineInfo,
   TextBlockStyle, TextBlockCursor,
   TextBlockNodeFormat, TextBlockCursorType,
   CallbackType(..), wrapEventCallback,
   ObjectEventCb, ObjectImagePixelsGetCb,
   Point(..), EventFlags, ButtonFlags,
   MouseDownEvent, MouseUpEvent, MouseInEvent, MouseOutEvent,
   MouseMoveEvent, MouseWheelEvent, 
   MultiDownEvent, MultiUpEvent, MultiMoveEvent,
   eventFlagIsScrolling, eventFlagIsHolding,
   buttonFlagIsDoubleClick, buttonFlagIstripleClick
) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Control.Applicative
import Data.Int (Int16)
import Data.Bits

#include <Evas.h>

#c
enum TextStyle
{
   TEXT_STYLE_PLAIN,
   TEXT_STYLE_SHADOW,
   TEXT_STYLE_OUTLINE,
   TEXT_STYLE_SOFT_OUTLINE,
   TEXT_STYLE_GLOW,
   TEXT_STYLE_OUTLINE_SHADOW,
   TEXT_STYLE_FAR_SHADOW,
   TEXT_STYLE_OUTLINE_SOFT_SHADOW,
   TEXT_STYLE_SOFT_SHADOW,
   TEXT_STYLE_FAR_SOFT_SHADOW
};

enum TextShadowStyle {
   TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_RIGHT = (0x0 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_BOTTOM = (0x1 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_LEFT = (0x2 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_LEFT = (0x3 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_TOP_LEFT = (0x4 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_TOP = (0x5 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_TOP_RIGHT = (0x6 << 4),
   TEXT_STYLE_SHADOW_DIRECTION_RIGHT = (0x7 << 4)
};

enum TextDirection
{
   BIDI_DIRECTION_NATURAL,
   BIDI_DIRECTION_LTR,
   BIDI_DIRECTION_RTL
};

#endc

-- We use custom enums as the original is in fact a bitset
{#enum TextStyle {underscoreToCase} deriving (Eq,Show) #}
{#enum TextShadowStyle {underscoreToCase} deriving (Eq,Show) #}
{#enum TextDirection {underscoreToCase} deriving (Eq,Show) #}

type Canvas = Ptr ()
type Object = Ptr ()
type Coord = CInt
type PixelImportSource = Ptr ()
type NativeSurface = Ptr ()
type VideoSurface = Ptr ()
type FontSize = CInt
type CallbackPriority = Int16
type Map = Ptr ()
type EngineInfo = Ptr ()
type TextBlockStyle  = Ptr ()
type TextBlockCursor  = Ptr ()
type TextBlockNodeFormat  = Ptr ()
type TextBlockCursorType  = Ptr ()
type MouseDownEvent = Ptr ()
type MouseUpEvent = Ptr ()
type MouseInEvent = Ptr ()
type MouseOutEvent = Ptr ()
type MouseMoveEvent = Ptr ()
type MouseWheelEvent = Ptr ()
type MultiDownEvent = Ptr ()
type MultiUpEvent = Ptr ()
type MultiMoveEvent = Ptr ()
type ModifierKeys = Ptr ()
type LockKeys = Ptr ()
type Device = Ptr ()
type EventFlags = CInt
type ButtonFlags = CInt

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Scale_Hint as ImageScaleHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Content_Hint as ImageContentHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Animated_Loop_Hint as ImageAnimatedLoopType {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Callback_Type as CallbackType {underscoreToCase} deriving (Eq,Show) #}


type ObjectEventCb = FunPtr (Ptr () -> Canvas -> Object -> Ptr () -> IO ())

foreign import ccall "wrapper" wrapEventCallback :: (Ptr () -> Canvas -> Object -> Ptr () -> IO ()) -> IO ObjectEventCb


type ObjectImagePixelsGetCb = FunPtr (Ptr () -> Object -> IO ())


data Point = Point CInt CInt deriving (Eq,Show)

instance Storable Point where
   alignment = sizeOf
   sizeOf _ = {# sizeof Evas_Point #}
   peek p = Point 
      <$> {# get Evas_Point->x #} p
      <*> {# get Evas_Point->y #} p
   poke p (Point x y) = do
      {# set Evas_Point->x #} p x
      {# set Evas_Point->y #} p y

-- | Test if Holding flag is set
eventFlagIsHolding :: EventFlags -> Bool
eventFlagIsHolding = flip testBit 1

-- | Test if Scrolling flag is set
eventFlagIsScrolling :: EventFlags -> Bool
eventFlagIsScrolling = flip testBit 2

-- | Test if second click of a double-click
buttonFlagIsDoubleClick :: ButtonFlags -> Bool
buttonFlagIsDoubleClick = flip testBit 1

-- | Test if second click of a triple-click
buttonFlagIstripleClick :: ButtonFlags -> Bool
buttonFlagIstripleClick = flip testBit 2
