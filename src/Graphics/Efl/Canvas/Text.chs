{-# Language ForeignFunctionInterface #-}

-- | Text
module Graphics.Efl.Canvas.Text (
   addText,
   setTextFontSource, getTextFontSource,
   setTextFont, getTextFont,
   setText, getText,
   setTextBidiDelimiters, getTextBidiDelimiters,
   getTextCharPos, getTextLastCharUpToPos,
   setTextStyle, getTextStyle,
   setTextShadowColor, getTextShadowColor,
   setTextGlowColor, getTextGlowColor,
   setTextGlow2Color, getTextGlow2Color,
   setTextOutlineColor, getTextOutlineColor,
   getTextStylePad,
   getTextDirection
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Applicative
import Data.Bits

import Graphics.Efl.Helpers
import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types

-- | Create a new text object on the provided canvas
foreign import ccall "evas_object_text_add" addText :: Canvas -> IO Object

-- | Set the font (source) file to be used on a given text object 
setTextFontSource :: String -> Object -> IO ()
setTextFontSource font obj = withCString font (_object_text_font_source_set obj)

foreign import ccall "evas_object_text_font_source_set" _object_text_font_source_set :: Object -> CString -> IO ()

-- | Get the font file's path which is being used on a given text object
getTextFontSource :: Object -> IO String
getTextFontSource obj = peekCString =<< _object_text_font_source_get obj

foreign import ccall "evas_object_text_font_source_get" _object_text_font_source_get :: Object -> IO CString

-- | Set the font family and size on a given text object
setTextFont :: String -> FontSize -> Object -> IO ()
setTextFont font size obj = withCString font (flip (_object_text_font_set obj) size)

foreign import ccall "evas_object_text_font_set" _object_text_font_set :: Object -> CString -> FontSize -> IO ()


-- | Retrieve the font family and size in use on a given text object
getTextFont :: Object -> IO (String,FontSize)
getTextFont obj = do
   (cfont, size) <- get2_helper (_object_text_font_get obj)
   font <- peekCString cfont
   return (font,size)

foreign import ccall "evas_object_text_font_get" _object_text_font_get :: Object -> Ptr CString -> Ptr FontSize -> IO ()

-- | Set the text string to be displayed by the given text object
setText :: String -> Object -> IO ()
setText text obj = withCString text (_object_text_text_set obj)

foreign import ccall "evas_object_text_text_set" _object_text_text_set :: Object -> CString -> IO ()


-- | Retrieve the text string currently being displayed by the given text object
getText :: Object -> IO String
getText obj = do
   s <- _object_text_text_get obj
   if s == nullPtr then return "" else peekCString s

foreign import ccall "evas_object_text_text_get" _object_text_text_get :: Object -> IO CString

-- | Set the BiDi delimiters used in the textblock
setTextBidiDelimiters :: String -> Object -> IO ()
setTextBidiDelimiters delim obj = withCString delim (_object_text_bidi_delimiters_set obj)

foreign import ccall "evas_object_text_bidi_delimiters_set" _object_text_bidi_delimiters_set :: Object -> CString -> IO ()


-- | Get the BiDi delimiters used in the textblock
getTextBidiDelimiters :: Object -> IO String
getTextBidiDelimiters obj = peekCString =<< _object_text_bidi_delimiters_get obj

foreign import ccall "evas_object_text_bidi_delimiters_get" _object_text_bidi_delimiters_get :: Object -> IO CString 


-- | Retrieve position and dimension information of a character within a text Evas_Object
getTextCharPos :: Object -> CInt -> IO (Maybe (Coord, Coord, Coord, Coord))
getTextCharPos obj pos = do
   (cx,cy,cw,ch,ret) <- get4_ex_helper (_object_text_char_pos_get obj pos)
   return $ if toBool ret  then Nothing else Just (cx,cy,cw,ch)

foreign import ccall "evas_object_text_char_pos_get" _object_text_char_pos_get :: Object -> CInt -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO EinaBool


-- | Return the logical position of the last char in the text up to the pos given
foreign import ccall "evas_object_text_last_up_to_pos" getTextLastCharUpToPos :: Object -> Coord -> Coord -> IO CInt

-- | Retrieve the style on use on the given text object
getTextStyle :: Object -> IO (TextStyle, TextShadowStyle)
getTextStyle obj = do
   v <- fromIntegral <$> _object_text_style_get obj
   return (toEnum (v .&. 0x0F), toEnum (v .&. 0xF0))

foreign import ccall "evas_object_text_style_get" _object_text_style_get :: Object -> IO CInt


-- | Set the style to apply on the given text object
setTextStyle :: TextStyle -> TextShadowStyle -> Object -> IO ()
setTextStyle style shadow obj = _object_text_style_set obj (fromIntegral (fromEnum style) .|. fromIntegral (fromEnum shadow))

foreign import ccall "evas_object_text_style_set" _object_text_style_set :: Object -> CInt -> IO ()


-- | Set the shadow color for the given text object
setTextShadowColor :: CInt -> CInt -> CInt -> CInt -> Object -> IO ()
setTextShadowColor r g b a obj = _setTextShadowColor obj r g b a

foreign import ccall "evas_object_text_shadow_color_set" _setTextShadowColor :: Object -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Retrieve the shadow color for the given text object
getTextShadowColor :: Object -> IO (CInt, CInt, CInt, CInt)
getTextShadowColor obj = get4_helper (_object_text_shadow_color_get obj)

foreign import ccall "evas_object_text_shadow_color_get" _object_text_shadow_color_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- | Set the glow color for the given text object
setTextGlowColor :: CInt -> CInt -> CInt -> CInt -> Object -> IO ()
setTextGlowColor r g b a obj = _setTextGlowColor obj r g b a

foreign import ccall "evas_object_text_glow_color_set" _setTextGlowColor :: Object -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Retrieve the glow color for the given text object
getTextGlowColor :: Object -> IO (CInt, CInt, CInt, CInt)
getTextGlowColor obj = get4_helper (_object_text_glow_color_get obj)

foreign import ccall "evas_object_text_glow_color_get" _object_text_glow_color_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- | Set the glow2 color for the given text object
setTextGlow2Color :: CInt -> CInt -> CInt -> CInt -> Object -> IO ()
setTextGlow2Color r g b a obj = _setTextGlow2Color obj r g b a

foreign import ccall "evas_object_text_glow2_color_set" _setTextGlow2Color :: Object -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Retrieve the glow2 color for the given text object
getTextGlow2Color :: Object -> IO (CInt, CInt, CInt, CInt)
getTextGlow2Color obj = get4_helper (_object_text_glow2_color_get obj)

foreign import ccall "evas_object_text_glow2_color_get" _object_text_glow2_color_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | Set the outline color for the given text object
setTextOutlineColor :: CInt -> CInt -> CInt -> CInt -> Object -> IO ()
setTextOutlineColor r g b a obj = _setTextOutlineColor obj r g b a

foreign import ccall "evas_object_text_outline_color_set" _setTextOutlineColor :: Object -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Retrieve the outline color for the given text object
getTextOutlineColor :: Object -> IO (CInt, CInt, CInt, CInt)
getTextOutlineColor obj = get4_helper (_object_text_outline_color_get obj)

foreign import ccall "evas_object_text_outline_color_get" _object_text_outline_color_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- | Gets the text style pad of a text object
getTextStylePad :: Object -> IO (CInt, CInt, CInt, CInt)
getTextStylePad obj = get4_helper (_object_text_style_pad_get obj)

foreign import ccall "evas_object_text_style_pad_get" _object_text_style_pad_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- | Retrieve the direction of the text currently being displayed in the text object 
getTextDirection :: Object -> IO TextDirection
getTextDirection obj = toEnum . fromIntegral <$> _object_text_direction_get obj

foreign import ccall "evas_object_text_direction_get" _object_text_direction_get :: Object -> IO CInt
