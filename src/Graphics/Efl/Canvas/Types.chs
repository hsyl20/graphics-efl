{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Types where

import Foreign.Ptr
import Data.Int (Int16)

#include <Evas.h>

#c
typedef enum _Evas_Text_Style
{
   EVAS_TEXT_STYLE_PLAIN,
   EVAS_TEXT_STYLE_SHADOW,
   EVAS_TEXT_STYLE_OUTLINE,
   EVAS_TEXT_STYLE_SOFT_OUTLINE,
   EVAS_TEXT_STYLE_GLOW,
   EVAS_TEXT_STYLE_OUTLINE_SHADOW,
   EVAS_TEXT_STYLE_FAR_SHADOW,
   EVAS_TEXT_STYLE_OUTLINE_SOFT_SHADOW,
   EVAS_TEXT_STYLE_SOFT_SHADOW,
   EVAS_TEXT_STYLE_FAR_SOFT_SHADOW
} Evas_Text_Style;

typedef enum _Evas_Text_Shadow_Style {
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_RIGHT = (0x0 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM = (0x1 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_LEFT = (0x2 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_LEFT = (0x3 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP_LEFT = (0x4 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP = (0x5 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP_RIGHT = (0x6 << 4),
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_RIGHT = (0x7 << 4)
} Evas_Text_Shadow_Style;

typedef enum _Evas_Text_Direction
{
   EVAS_BIDI_DIRECTION_NATURAL,
   EVAS_BIDI_DIRECTION_LTR,
   EVAS_BIDI_DIRECTION_RTL
} Evas_Text_Direction;

#endc

-- We use custom enums as the original is in fact a bitset
{#enum _Evas_Text_Style as TextStyle {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Text_Shadow_Style as TextShadowStyle {underscoreToCase} deriving (Eq,Show) #}

type Canvas = Ptr ()
type Object = Ptr ()
type Coord = Int
type PixelImportSource = Ptr ()
type NativeSurface = Ptr ()
type VideoSurface = Ptr ()
type FontSize = Int
type CallbackPriority = Int16
type Map = Ptr ()

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Scale_Hint as ImageScaleHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Content_Hint as ImageContentHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Animated_Loop_Hint as ImageAnimatedLoopType {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Text_Direction as TextDirection {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Callback_Type as CallbackType {underscoreToCase} deriving (Eq,Show) #}


type ObjectEventCb = FunPtr (Ptr () -> Canvas -> Object -> Ptr () -> IO ())

type ObjectImagePixelsGetCb = FunPtr (Ptr () -> Object -> IO ())
