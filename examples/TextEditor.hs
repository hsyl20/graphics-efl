import Graphics.Efl.Simple
import Control.Applicative
import Data.IORef

main :: IO ()
main = do
   initWindowingSystem
   engines <- getEngines
   putStrLn (show engines)

   let engine = if "opengl_x11" `elem` engines 
         then Just "opengl_x11"
         else Nothing

   withDefaultWindow engine $ \ win canvas -> do
      engineName <- getEngineName win
      putStrLn $ "Using engine " ++ engineName

      setWindowTitle "Haskell-EFL Text Editor" win

      bg <- addRectangle canvas
            |> setObjectColor (0,0,0,255)
            |> setLayer (-1)
            |> uncover

      let introText = "Welcome into this new text editor"

      style <- createTextBlockStyle
                  |> configureTextBlockStyle "DEFAULT='font=DejaVuSans \
                                             \font_size=16 align=left \
                                             \color=#FFFFFF wrap=word \
                                             \style=plain \
                                             \shadow_color=#CCCCCC' \
                                             \NewLine= '+\n'"

      state <- newIORef initState

      let 
         runEvent :: Object -> EditorEvent -> IO ()
         runEvent self ev = readIORef state >>= handleEvent self ev >>= writeIORef state

      textArea <- addTextBlock canvas
            |> setTextBlockStyle style
            |> setTextBlockTextMarkup introText
            |> resize 500 400
            |> setObjectColor (255,255,255,255)
            |> setFocus True
            |> uncover
            |> onKeyDown (\self -> runEvent self . EditorKeyDown)


      onWindowResize win $ do
         (_,_,w,h) <- getWindowGeometry win
         resize w h bg
         resize w h textArea

      return ()


data EditorEvent
   = EditorKeyDown KeyDownEvent

data EditorState = EditorState {
   editorMode :: EditorMode
}

data EditorMode
   = CommandMode String
   | InsertMode
   | NormalMode

initState :: EditorState
initState = EditorState 
   { editorMode = NormalMode
   }


handleEvent :: Object -> EditorEvent -> EditorState -> IO EditorState
handleEvent textArea event state = do

   case editorMode state of
      InsertMode -> case event of
         EditorKeyDown ev -> do
            name <- keyDownKeyName ev
            old <- getTextBlockTextMarkup textArea
            case name of
               "Escape" -> return $ EditorState $ NormalMode
               s -> do
                  new <- case s of
                     "BackSpace" -> return $ if null old then old else init old
                     "Return" -> return (old ++ "<br/>")
                     _ -> (old ++) <$> keyDownString ev
                  setTextBlockTextMarkup new textArea
                  return state

      CommandMode cmd -> case event of
         EditorKeyDown ev -> do
            name <- keyDownKeyName ev
            case name of
               "Escape"    -> return $ EditorState NormalMode
               "BackSpace" -> return $ EditorState (CommandMode $ if null cmd then cmd else init cmd)
               "Return"    -> case cmd of
                                 "quit" -> quitMainLoop >> (return $ EditorState $ NormalMode)
                                 "q"    -> quitMainLoop >> (return $ EditorState $ NormalMode)
                                 c      -> putStrLn ("Unrecognized command " ++ c) >> (return $ EditorState $ NormalMode)
               _           -> EditorState . CommandMode . (++) cmd <$> keyDownString ev

      NormalMode -> case event of
         EditorKeyDown ev -> do
            name <- keyDownKeyName ev
            str <- keyDownString ev
            case (name,str) of
               (_,":")     -> return $ EditorState $ CommandMode ""
               (_,"i")     -> return $ EditorState $ InsertMode
               _           -> return state
