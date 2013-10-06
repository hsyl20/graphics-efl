{-# Language ForeignFunctionInterface #-}

-- | EFL types
module Graphics.Efl.Canvas.Types (
   TextStyle(..), TextShadowStyle(..),
   BorderFillMode(..), FillSpread(..), LoadError(..),
   ColorSpace(..), ImageScaleHint(..), ImageContentHint(..),
   ImageAnimatedLoopType(..), TextDirection(..),
   Canvas, Object, Coord,
   PixelImportSource, NativeSurface, VideoSurface,
   FontSize, CallbackPriority, Map, EngineInfo,
   TextBlockStyle, TextBlockCursor,
   TextBlockNodeFormat,
   CallbackType(..), 
   ObjectEventCb, ObjectImagePixelsGetCb,
   EventFlags(..)
) where

import Foreign.Ptr
import Data.Int (Int16)

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

enum EventFlags
{
   EVENT_FLAG_NONE = 0,
   EVENT_FLAG_ON_HOLD = (1 << 0),
   EVENT_FLAG_ON_SCROLL = (1 << 1)
};

#endc

-- We use custom enums as the original is in fact a bitset
{#enum TextStyle {underscoreToCase} deriving (Eq,Show) #}
{#enum TextShadowStyle {underscoreToCase} deriving (Eq,Show) #}
{#enum EventFlags {underscoreToCase} deriving (Eq,Show) #}
{#enum TextDirection {underscoreToCase} deriving (Eq,Show) #}

type Canvas = Ptr ()
type Object = Ptr ()
type Coord = Int
type PixelImportSource = Ptr ()
type NativeSurface = Ptr ()
type VideoSurface = Ptr ()
type FontSize = Int
type CallbackPriority = Int16
type Map = Ptr ()
type EngineInfo = Ptr ()
type TextBlockStyle  = Ptr ()
type TextBlockCursor  = Ptr ()
type TextBlockNodeFormat  = Ptr ()

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Scale_Hint as ImageScaleHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Content_Hint as ImageContentHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Animated_Loop_Hint as ImageAnimatedLoopType {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Callback_Type as CallbackType {underscoreToCase} deriving (Eq,Show) #}


type ObjectEventCb = FunPtr (Ptr () -> Canvas -> Object -> Ptr () -> IO ())

type ObjectImagePixelsGetCb = FunPtr (Ptr () -> Object -> IO ())
