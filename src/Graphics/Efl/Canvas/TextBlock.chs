{-# Language ForeignFunctionInterface #-}

-- | Text
module Graphics.Efl.Canvas.TextBlock (
   addTextBlock, 
   unescapeTextBlockString, escapeTextBlockString,
   unescapeTextBlockStringRange,
   getTextBlockPlainText, getTextBlockMarkup,
   createTextBlockStyle, destroyTextBlockStyle,
   configureTextBlockStyle, getTextBlockStyleConfiguration,
   setTextBlockStyle, getTextBlockStyle,
   pushTextBlockStyle, popTextBlockStyle, peekTextBlockStyle,
   setTextBlockReplaceChar, getTextBlockReplaceChar,
   setTextBlockVAlign, getTextBlockVAlign,
   setTextBlockBidiDelimiters, getTextBlockBidiDelimiters,
   setTextBlockLegacyNewline, getTextBlockLegacyNewline,
   setTextBlockTextMarkup, getTextBlockTextMarkup,
   prependTextBlockTextMarkup, getTextBlockCursor,
   createTextBlockCursor, destroyTextBlockCursor,
   setTextBlockCursorFirstParagraph, setTextBlockCursorLastParagraph,
   setTextBlockCursorNextParagraph, setTextBlockCursorPreviousParagraph,
   getTextBlockFormatNodeList, getTextBlockFirstFormatNode, getTextBlockLastFormatNode,
   getTextBlockNextFormatNode, getTextBlockPreviousFormatNode
) where

import Foreign.C.String
import Foreign.C.Types
import Control.Applicative

import Graphics.Efl.Canvas.Types
import Graphics.Efl.Eina
import Graphics.Efl.Helpers

-- | Create a new text block object on the provided canvas
foreign import ccall "evas_object_textblock_add" addTextBlock :: Canvas -> IO Object

-- | Return the unescaped version of escape
foreign import ccall "evas_textblock_escape_string_get" unescapeTextBlockString :: CString -> IO CString

-- | Return the escaped version of the string
foreign import ccall "evas_textblock_string_escape_get" escapeTextBlockString :: CString -> IO CString

-- | Return the unescaped version of the string between start and end
foreign import ccall "evas_textblock_escape_string_range_get" unescapeTextBlockStringRange :: CString -> CString -> IO CString

-- | Return the plain version of the markup
foreign import ccall "evas_textblock_text_markup_to_utf8" getTextBlockPlainText :: Object -> CString -> IO CString

-- | Return the markup version of the plain text
foreign import ccall "evas_textblock_text_utf8_to_markup" getTextBlockMarkup :: Object -> CString -> IO CString

-- | Create a new textblock style
foreign import ccall "evas_textblock_style_new" createTextBlockStyle :: IO TextBlockStyle

-- | Destroy a textblock style
foreign import ccall "evas_textblock_style_free" destroyTextBlockStyle :: TextBlockStyle -> IO ()

-- | Configure textblock style
foreign import ccall "evas_textblock_style_set" configureTextBlockStyle :: TextBlockStyle -> CString -> IO ()

-- | Get textblock style configuration
foreign import ccall "evas_textblock_style_get" getTextBlockStyleConfiguration :: TextBlockStyle -> IO CString

-- | Set text block style
foreign import ccall "evas_object_textblock_style_set" setTextBlockStyle :: Object -> TextBlockStyle -> IO ()

-- | Get text block style
foreign import ccall "evas_object_textblock_style_get" getTextBlockStyle :: Object -> IO TextBlockStyle

-- | Push style to the top of the user style stack
foreign import ccall "evas_object_textblock_style_user_push" pushTextBlockStyle :: Object -> TextBlockStyle -> IO ()

-- | Delete the top of the user style stack
foreign import ccall "evas_object_textblock_style_user_pop" popTextBlockStyle :: Object -> IO ()

-- | Peek the top of the user style stack
foreign import ccall "evas_object_textblock_style_user_peek" peekTextBlockStyle :: Object -> IO TextBlockStyle

-- | Set the "replacement character" to use for the given textblock object
foreign import ccall "evas_object_textblock_replace_char_set" setTextBlockReplaceChar :: Object -> CString -> IO ()

-- | Get the "replacement character" to use for the given textblock object
foreign import ccall "evas_object_textblock_replace_char_get" getTextBlockReplaceChar :: Object -> IO CString

-- | Set the vertical alignment of text within the textblock object as a whole
foreign import ccall "evas_object_textblock_valign_set" setTextBlockVAlign :: Object -> Double -> IO ()

-- | Get the vertical alignment of text within the textblock object as a whole
foreign import ccall "evas_object_textblock_valign_get" getTextBlockVAlign :: Object -> IO Double

-- | Set the BiDi delimiters used in the textblock
foreign import ccall "evas_object_textblock_bidi_delimiters_set" setTextBlockBidiDelimiters :: Object -> CString -> IO ()

-- | Get the BiDi delimiters used in the textblock
foreign import ccall "evas_object_textblock_bidi_delimiters_get" getTextBlockBidiDelimiters :: Object -> IO CString

-- | Set newline mode. When true, newline character will behave as a paragraph separator
setTextBlockLegacyNewline :: Object -> Bool -> IO ()
setTextBlockLegacyNewline obj mode = _setTextBlockLegacyNewline obj (fromBool mode)

foreign import ccall "evas_object_textblock_legacy_newline_set" _setTextBlockLegacyNewline :: Object -> EinaBool -> IO ()

-- | Get newline mode. When true, newline character behaves as a paragraph separator
getTextBlockLegacyNewline :: Object -> IO Bool
getTextBlockLegacyNewline obj = toBool <$> _getTextBlockLegacyNewline obj

foreign import ccall "evas_object_textblock_legacy_newline_get" _getTextBlockLegacyNewline :: Object -> IO EinaBool

-- | Set the tetxblock's text to the markup text
foreign import ccall "evas_object_textblock_text_markup_set" setTextBlockTextMarkup :: Object -> CString -> IO ()

-- | Prepend markup to the cursor cur
foreign import ccall "evas_object_textblock_text_markup_prepend" prependTextBlockTextMarkup :: TextBlockCursor -> CString -> IO ()

-- | Return the markup of the object
foreign import ccall "evas_object_textblock_text_markup_get" getTextBlockTextMarkup :: Object -> IO CString

-- | Return the object's main cursor
foreign import ccall "evas_object_textblock_cursor_get" getTextBlockCursor :: Object -> IO TextBlockCursor

-- | Create a new cursor, associate it to the obj and init it to point
-- to the start of the textblock. Association to the object means the cursor
-- will be updated when the object will change.
foreign import ccall "evas_object_textblock_cursor_new" createTextBlockCursor :: Object -> IO TextBlockCursor

-- | Free the cursor and unassociate it from the object
-- Do not use it to free unassociated cursors.
foreign import ccall "evas_textblock_cursor_free" destroyTextBlockCursor :: TextBlockCursor -> IO ()

-- | Set the cursor to the start of the first text node
foreign import ccall "evas_textblock_cursor_paragraph_first" setTextBlockCursorFirstParagraph :: TextBlockCursor -> IO ()

-- | Set the cursor to the end of the last text node
foreign import ccall "evas_textblock_cursor_paragraph_last" setTextBlockCursorLastParagraph :: TextBlockCursor -> IO ()

-- | Advance cursor to the start of the next text node
setTextBlockCursorNextParagraph :: TextBlockCursor -> IO Bool
setTextBlockCursorNextParagraph cur = toBool <$> _setTextBlockCursorNextParagraph cur

foreign import ccall "evas_textblock_cursor_paragraph_next" _setTextBlockCursorNextParagraph :: TextBlockCursor -> IO EinaBool

-- | Advance cursor to the end of the previous text node
setTextBlockCursorPreviousParagraph :: TextBlockCursor -> IO Bool
setTextBlockCursorPreviousParagraph cur = toBool <$> _setTextBlockCursorPreviousParagraph cur

foreign import ccall "evas_textblock_cursor_paragraph_prev" _setTextBlockCursorPreviousParagraph :: TextBlockCursor -> IO EinaBool


-- | Return the node format list
foreign import ccall "evas_textblock_node_format_list_get" getTextBlockFormatNodeList :: Object -> IO (EinaList TextBlockNodeFormat)

-- | Return the first format node
foreign import ccall "evas_textblock_node_format_first_get" getTextBlockFirstFormatNode :: Object -> IO TextBlockNodeFormat

-- | Return the last format node
foreign import ccall "evas_textblock_node_format_last_get" getTextBlockLastFormatNode :: Object -> IO TextBlockNodeFormat

-- | Return the next format node
getTextBlockNextFormatNode :: TextBlockNodeFormat -> IO (Maybe TextBlockNodeFormat)
getTextBlockNextFormatNode n = maybePtr <$>  _getTextBlockNextFormatNode n

foreign import ccall "evas_textblock_node_format_next_get" _getTextBlockNextFormatNode :: TextBlockNodeFormat -> IO TextBlockNodeFormat

-- | Return the prev format node
getTextBlockPreviousFormatNode :: TextBlockNodeFormat -> IO (Maybe TextBlockNodeFormat)
getTextBlockPreviousFormatNode n = maybePtr <$>  _getTextBlockPreviousFormatNode n

foreign import ccall "evas_textblock_node_format_prev_get" _getTextBlockPreviousFormatNode :: TextBlockNodeFormat -> IO TextBlockNodeFormat
