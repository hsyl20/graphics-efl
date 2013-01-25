{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Evas.Types where

import Foreign.Ptr

#include <Evas.h>

type Evas = Ptr ()
type Object = Ptr ()
type Coord = Int
type PixelImportSource = Ptr ()
type NativeSurface = Ptr ()
type VideoSurface = Ptr ()

{#enum _Evas_Border_Fill_Mode as BorderFillMode {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Fill_Spread as FillSpread {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Load_Error as LoadError {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Colorspace as ColorSpace {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Scale_Hint as ImageScaleHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Content_Hint as ImageContentHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Image_Animated_Loop_Hint as ImageAnimatedLoopHint {underscoreToCase} deriving (Eq,Show) #}
{#enum _Evas_Callback_Type as CallbackType {underscoreToCase} deriving (Eq,Show) #}


type ObjectEventCb = FunPtr (Ptr () -> Evas -> Object -> Ptr () -> IO ())

type ObjectImagePixelsGetCb = FunPtr (Ptr () -> Object -> IO ())
