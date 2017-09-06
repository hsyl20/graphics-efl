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
   getTextBlockNextFormatNode, getTextBlockPreviousFormatNode,
   removeTextBlockFormatNodePair, setTextBlockCursorAtFormatNode,
   getTextBlockFormatNodeAtCursor, getTextBlockFormatNodeText,
   setTextBlockCursorAtFormatPosition, isTextBlockCursorFormatVisible,
   advanceTextBlockCursorToNextFormat, advanceTextBlockCursorToPreviousFormat,
   isTextBlockCursorFormat,
   advanceTextBlockCursorPreviousChar, advanceTextBlockCursorNextChar,
   moveTextBlockCursorWordStart, moveTextBlockCursorWordEnd,
   moveTextBlockCursorParagraphFirstChar, moveTextBlockCursorParagraphLastChar,
   moveTextBlockCursorLineFirstChar, moveTextBlockCursorLineLastChar,
   getTextBlockCursorPos, setTextBlockCursorPos, setTextBlockCursorLine,
   compareTextBlockCursors, copyTextBlockCursors,
   appendTextBlockCursorText, prependTextBlockCursorText,
   appendTextBlockCursorFormat, prependTextBlockCursorFormat,
   deleteTextBlockCursorChar, deleteTextBlockCursorRange,
   getTextBlockCursorParagraphText, getTextBlockCursorParagraphTextLength,
   getTextBlockCursorVisibleRange, getTextBlockCursorRangeFormats,
   getTextBlockCursorRangeText, getTextBlockCursorContent,
   getTextBlockCursorGeometry, getTextBlockCursorCharGeometry,
   getTextBlockCursorPenGeometry, getTextBlockCursorLineGeometry,
   getTextBlockCursorRangeGeometry, getTextBlockCursorFormatItemGeometry,
   setTextBlockCursorCharCoord, setTextBlockCursorLineCoord,
   isTextBlockCursorEndOfLine, getTextBlockLineGeometry,
   clearTextBlock, getTextBlockFormattedSize, getTextBlockNativeSize
) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Control.Monad

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
configureTextBlockStyle :: String -> TextBlockStyle -> IO ()
configureTextBlockStyle s sty = withCString s (_configureTextBlockStyle sty)

foreign import ccall "evas_textblock_style_set" _configureTextBlockStyle :: TextBlockStyle -> CString -> IO ()

-- | Get textblock style configuration
foreign import ccall "evas_textblock_style_get" getTextBlockStyleConfiguration :: TextBlockStyle -> IO CString

-- | Set text block style
setTextBlockStyle :: TextBlockStyle -> Object -> IO ()
setTextBlockStyle = flip _setTextBlockStyle

foreign import ccall "evas_object_textblock_style_set" _setTextBlockStyle :: Object -> TextBlockStyle -> IO ()

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
setTextBlockTextMarkup :: String -> Object -> IO ()
setTextBlockTextMarkup s obj = withCString s (_setTextBlockTextMarkup obj)

foreign import ccall "evas_object_textblock_text_markup_set" _setTextBlockTextMarkup :: Object -> CString -> IO ()

-- | Prepend markup to the cursor cur
foreign import ccall "evas_object_textblock_text_markup_prepend" prependTextBlockTextMarkup :: TextBlockCursor -> CString -> IO ()

-- | Return the markup of the object
getTextBlockTextMarkup :: Object -> IO String
getTextBlockTextMarkup = peekCString <=< _getTextBlockTextMarkup

foreign import ccall "evas_object_textblock_text_markup_get" _getTextBlockTextMarkup :: Object -> IO CString

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

-- | Remove a format node and it's match. i.e, removes a \<tag\> \</tag\> pair.
-- Assumes the node is the first part of \<tag\> i.e, this won't work if
-- n is a closing tag.
foreign import ccall "evas_textblock_node_format_remove_pair" removeTextBlockFormatNodePair :: Object -> TextBlockNodeFormat -> IO ()

-- | Set the cursor to point to the place where format points to
foreign import ccall "evas_textblock_cursor_set_at_format" setTextBlockCursorAtFormatNode :: TextBlockCursor -> TextBlockNodeFormat -> IO ()

-- | Return the format node at the position pointed by cur
foreign import ccall "evas_textblock_cursor_format_get" getTextBlockFormatNodeAtCursor :: TextBlockCursor -> IO TextBlockNodeFormat

-- | Get the text format representation of the format node
foreign import ccall "evas_textblock_node_format_text_get" getTextBlockFormatNodeText :: TextBlockNodeFormat -> IO CString

-- | Set the cursor to point to the position of fmt
foreign import ccall "evas_textblock_cursor_at_format_set" setTextBlockCursorAtFormatPosition :: TextBlockCursor -> TextBlockNodeFormat -> IO ()

-- | Check if the current cursor position is a visible format. This way is more
-- efficient than evas_textblock_cursor_format_get() to check for the existence
-- of a visible format.
isTextBlockCursorFormatVisible :: TextBlockCursor -> IO Bool
isTextBlockCursorFormatVisible cur = toBool <$> _isTextBlockCursorFormatVisible cur

foreign import ccall "evas_textblock_cursor_format_is_visible_get" _isTextBlockCursorFormatVisible :: TextBlockCursor -> IO EinaBool

-- | Advance to the next format node
advanceTextBlockCursorToNextFormat :: TextBlockCursor -> IO Bool
advanceTextBlockCursorToNextFormat cur = toBool <$> _advanceTextBlockCursorToNextFormat cur

foreign import ccall "evas_textblock_cursor_format_next" _advanceTextBlockCursorToNextFormat :: TextBlockCursor -> IO EinaBool

-- | Advance to the prev format node
advanceTextBlockCursorToPreviousFormat :: TextBlockCursor -> IO Bool
advanceTextBlockCursorToPreviousFormat cur = toBool <$> _advanceTextBlockCursorToPreviousFormat cur

foreign import ccall "evas_textblock_cursor_format_prev" _advanceTextBlockCursorToPreviousFormat :: TextBlockCursor -> IO EinaBool

-- | Return true if the cursor points to a format
isTextBlockCursorFormat :: TextBlockCursor -> IO Bool
isTextBlockCursorFormat cur = toBool <$> _isTextBlockCursorFormatVisible cur

foreign import ccall "evas_textblock_cursor_is_format" _isTextBlockCursorFormat :: TextBlockCursor -> IO EinaBool

-- | Advance 1 char forward
advanceTextBlockCursorNextChar :: TextBlockCursor -> IO Bool
advanceTextBlockCursorNextChar cur = toBool <$> _advanceTextBlockCursorNextChar cur

foreign import ccall "evas_textblock_cursor_char_next" _advanceTextBlockCursorNextChar :: TextBlockCursor -> IO EinaBool

-- | Advance 1 char backward
advanceTextBlockCursorPreviousChar :: TextBlockCursor -> IO Bool
advanceTextBlockCursorPreviousChar cur = toBool <$> _advanceTextBlockCursorPreviousChar cur

foreign import ccall "evas_textblock_cursor_char_prev" _advanceTextBlockCursorPreviousChar :: TextBlockCursor -> IO EinaBool

-- | Move the cursor to the start of the word under the cursor
moveTextBlockCursorWordStart :: TextBlockCursor -> IO Bool
moveTextBlockCursorWordStart cur = toBool <$> _moveTextBlockCursorWordStart cur

foreign import ccall "evas_textblock_cursor_word_start" _moveTextBlockCursorWordStart :: TextBlockCursor -> IO EinaBool

-- | Move the cursor to the end of the word under the cursor
moveTextBlockCursorWordEnd :: TextBlockCursor -> IO Bool
moveTextBlockCursorWordEnd cur = toBool <$> _moveTextBlockCursorWordEnd cur

foreign import ccall "evas_textblock_cursor_word_end" _moveTextBlockCursorWordEnd :: TextBlockCursor -> IO EinaBool

-- | Go to the first char in the node the cursor is pointing on
foreign import ccall "evas_textblock_cursor_paragraph_char_first" moveTextBlockCursorParagraphFirstChar :: TextBlockCursor -> IO ()

-- | Go to the last char in a text node
foreign import ccall "evas_textblock_cursor_paragraph_char_last" moveTextBlockCursorParagraphLastChar :: TextBlockCursor -> IO ()

-- | Go to the start of the current line
foreign import ccall "evas_textblock_cursor_line_char_first" moveTextBlockCursorLineFirstChar :: TextBlockCursor -> IO ()

-- | Go to the end of the current line
foreign import ccall "evas_textblock_cursor_line_char_last" moveTextBlockCursorLineLastChar :: TextBlockCursor -> IO ()

-- | Return the current cursor pos
foreign import ccall "evas_textblock_cursor_pos_get" getTextBlockCursorPos :: TextBlockCursor -> IO CInt

-- | Set the cursor pos
foreign import ccall "evas_textblock_cursor_pos_set" setTextBlockCursorPos :: TextBlockCursor -> CInt -> IO ()

-- | Go to the start of the line passed
foreign import ccall "evas_textblock_cursor_line_set" setTextBlockCursorLine :: TextBlockCursor -> CInt -> IO ()

-- | Compare two cursors
foreign import ccall "evas_textblock_cursor_compare" compareTextBlockCursors :: TextBlockCursor -> TextBlockCursor -> IO CInt

-- | Make cur_dest point to the same place as cur. Does not work if they don't point to the same object.
foreign import ccall "evas_textblock_cursor_copy" copyTextBlockCursors :: TextBlockCursor -> TextBlockCursor -> IO ()

-- | Add text to the current cursor position and set the cursor to *before* the start of the text just added
foreign import ccall "evas_textblock_cursor_text_append" appendTextBlockCursorText :: TextBlockCursor -> CString -> IO CInt

-- | Add text to the current cursor position and set the cursor to *after* the start of the text just added
foreign import ccall "evas_textblock_cursor_text_prepend" prependTextBlockCursorText :: TextBlockCursor -> CString -> IO CInt

-- | Add format to the current cursor position. If the format being added is a
-- visible format, add it *before* the cursor position, otherwise, add it after.
appendTextBlockCursorFormat :: TextBlockCursor -> String -> IO Bool
appendTextBlockCursorFormat cur fmt = toBool <$> withCString fmt (_appendTextBlockCursorFormat cur)

foreign import ccall "evas_textblock_cursor_format_append" _appendTextBlockCursorFormat :: TextBlockCursor -> CString -> IO EinaBool

-- | Add format to the current cursor position. If the format being added is a
-- visible format, add it *before* the cursor position, otherwise, add it after.
prependTextBlockCursorFormat :: TextBlockCursor -> String -> IO Bool
prependTextBlockCursorFormat cur fmt = toBool <$> withCString fmt (_prependTextBlockCursorFormat cur)

foreign import ccall "evas_textblock_cursor_format_prepend" _prependTextBlockCursorFormat :: TextBlockCursor -> CString -> IO EinaBool

-- | Delete the character at the location of the cursor. If there's a format pointing to this position, delete it as well.
foreign import ccall "evas_textblock_cursor_char_delete" deleteTextBlockCursorChar :: TextBlockCursor -> IO ()

-- | Delete the range between cur1 and cur2
foreign import ccall "evas_textblock_cursor_range_delete" deleteTextBlockCursorRange :: TextBlockCursor -> TextBlockCursor -> IO ()

-- | Return the text of the paragraph cur points to - returns the text in markup.
foreign import ccall "evas_textblock_cursor_paragraph_text_get" getTextBlockCursorParagraphText :: TextBlockCursor -> IO CString

-- | Return the length of the paragraph, cheaper the eina_unicode_strlen()
foreign import ccall "evas_textblock_cursor_paragraph_text_length_get" getTextBlockCursorParagraphTextLength :: TextBlockCursor -> IO CInt

-- | Return the currently visible range
getTextBlockCursorVisibleRange :: TextBlockCursor -> TextBlockCursor -> IO Bool
getTextBlockCursorVisibleRange cur1 cur2 = toBool <$> _getTextBlockCursorVisibleRange cur1 cur2

foreign import ccall "evas_textblock_cursor_visible_range_get" _getTextBlockCursorVisibleRange :: TextBlockCursor -> TextBlockCursor -> IO EinaBool

-- | Return the format nodes in the range between cur1 and cur2
foreign import ccall "evas_textblock_cursor_range_formats_get" getTextBlockCursorRangeFormats :: TextBlockCursor -> TextBlockCursor -> IO (EinaList TextBlockNodeFormat)

-- | Return the text in the range between cur1 and cur2
foreign import ccall "evas_textblock_cursor_range_text_get" getTextBlockCursorRangeText :: TextBlockCursor -> TextBlockCursor -> IO CString

-- | Return the content of the cursor
foreign import ccall "evas_textblock_cursor_content_get" getTextBlockCursorContent :: TextBlockCursor -> IO CString

-- | Return the geometry of the cursor. Depends on the type of cursor requested.
-- This should be used instead of char_geometry_get because there are weird special cases with BiDi text.
getTextBlockCursorGeometry :: TextBlockCursor -> TextDirection -> TextBlockCursorType -> IO (Coord,Coord,Coord,Coord,CInt)
getTextBlockCursorGeometry cur dir ctype = get4_ex_helper f
   where
      f cx cy cw ch = _getTextBlockCursorGeometry cur cx cy cw ch (fromIntegral $ fromEnum dir) ctype



foreign import ccall "evas_textblock_cursor_geometry_get" _getTextBlockCursorGeometry :: TextBlockCursor -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> CInt -> TextBlockCursorType -> IO CInt

-- | Return the geometry of the char at cur
foreign import ccall "evas_textblock_cursor_char_geometry_get" getTextBlockCursorCharGeometry :: TextBlockCursor -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO CInt

-- | Return the geometry of the pen at cur
foreign import ccall "evas_textblock_cursor_pen_geometry_get" getTextBlockCursorPenGeometry :: TextBlockCursor -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO CInt

-- | Return the geometry of the line at cur
foreign import ccall "evas_textblock_cursor_line_geometry_get" getTextBlockCursorLineGeometry :: TextBlockCursor -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO CInt

-- | Return the geometry of a range
foreign import ccall "evas_textblock_cursor_range_geometry_get" getTextBlockCursorRangeGeometry :: TextBlockCursor -> TextBlockCursor -> IO (EinaList Coord)

-- | Return the geometry of a format item
foreign import ccall "evas_textblock_cursor_format_item_geometry_get" getTextBlockCursorFormatItemGeometry :: TextBlockCursor -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO EinaBool

-- | Set the position of the cursor according to the X and Y coordinates
foreign import ccall "evas_textblock_cursor_char_coord_set" setTextBlockCursorCharCoord :: TextBlockCursor -> Coord -> Coord -> IO EinaBool

-- | Set the cursor position according to the y coord
foreign import ccall "evas_textblock_cursor_line_coord_set" setTextBlockCursorLineCoord :: TextBlockCursor -> Coord -> IO CInt

-- | Check if the cursor points to the end of the line
isTextBlockCursorEndOfLine :: TextBlockCursor -> IO Bool
isTextBlockCursorEndOfLine cur = toBool <$> _isTextBlockCursorEndOfLine cur

foreign import ccall "evas_textblock_cursor_eol_get" _isTextBlockCursorEndOfLine :: TextBlockCursor -> IO EinaBool

-- | Return the geometry of the given line
foreign import ccall "evas_object_textblock_line_number_geometry_get" getTextBlockLineGeometry :: Object -> CInt -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO EinaBool

-- | Clear the textblock object
foreign import ccall "evas_object_textblock_clear" clearTextBlock :: Object -> IO ()

-- | Get the formatted width and height
foreign import ccall "evas_object_textblock_size_formatted_get" getTextBlockFormattedSize :: Object -> Ptr Coord -> Ptr Coord -> IO ()

-- | Get the native width and height
foreign import ccall "evas_object_textblock_size_native_get" getTextBlockNativeSize :: Object -> Ptr Coord -> Ptr Coord -> IO ()
