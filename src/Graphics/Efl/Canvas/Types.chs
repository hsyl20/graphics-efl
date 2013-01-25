{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Canvas.Types where

import Foreign.Ptr

#include <Evas.h>

#c
typedef enum _Evas_Text_Style
{
   EVAS_TEXT_STYLE_PLAIN,      /**< plain, standard text */
   EVAS_TEXT_STYLE_SHADOW,      /**< text with shadow underneath */
   EVAS_TEXT_STYLE_OUTLINE,      /**< text with an outline */
   EVAS_TEXT_STYLE_SOFT_OUTLINE,      /**< text with a soft outline */
   EVAS_TEXT_STYLE_GLOW,      /**< text with a glow effect */
   EVAS_TEXT_STYLE_OUTLINE_SHADOW,      /**< text with both outline and shadow effects */
   EVAS_TEXT_STYLE_FAR_SHADOW,      /**< text with (far) shadow underneath */
   EVAS_TEXT_STYLE_OUTLINE_SOFT_SHADOW,      /**< text with outline and soft shadow effects combined */
   EVAS_TEXT_STYLE_SOFT_SHADOW,      /**< text with (soft) shadow underneath */
   EVAS_TEXT_STYLE_FAR_SOFT_SHADOW,      /**< text with (far soft) shadow underneath */
} Evas_Text_Style;

typedef enum _Evas_Text_Shadow_Style {
   /* OR these to modify shadow direction (3 bits needed) */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_RIGHT = (0x0 << 4),      /**< shadow growing to bottom right */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM = (0x1 << 4),            /**< shadow growing to the bottom */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_BOTTOM_LEFT = (0x2 << 4),       /**< shadow growing to bottom left */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_LEFT = (0x3 << 4),              /**< shadow growing to the left */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP_LEFT = (0x4 << 4),          /**< shadow growing to top left */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP = (0x5 << 4),               /**< shadow growing to the top */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_TOP_RIGHT = (0x6 << 4),         /**< shadow growing to top right */
   EVAS_TEXT_STYLE_SHADOW_DIRECTION_RIGHT = (0x7 << 4)             /**< shadow growing to the right */
} Evas_Text_Shadow_Style;

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

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Scale_Hint as ImageScaleHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Content_Hint as ImageContentHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Animated_Loop_Hint as ImageAnimatedLoopHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_BiDi_Direction as BiDiDirection {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Callback_Type as CallbackType {underscoreToCase} deriving (Eq,Show) #}


type ObjectEventCb = FunPtr (Ptr () -> Canvas -> Object -> Ptr () -> IO ())

type ObjectImagePixelsGetCb = FunPtr (Ptr () -> Object -> IO ())
