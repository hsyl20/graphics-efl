{-# Language ForeignFunctionInterface #-}

-- | Rendering engine
module Graphics.Efl.Canvas.Engine (
   lookupEngine, listEngines,
   setCanvasEngineById, getCanvasEngineId, getCanvasEngineName,
   getCanvasEngineInfo
) where

import Foreign.C.String

import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types


-- | Look up a numeric ID from a string name of a rendering engine.
lookupEngine :: String -> IO Int
lookupEngine name = withCString name _lookupEngine

foreign import ccall "evas_render_method_lookup" _lookupEngine :: CString -> IO Int

-- | List all the rendering engines compiled into the copy of the Evas library
listEngines :: IO [String]
listEngines = do
   xs <- _listEngines
   xs' <- mapM peekCString =<< toList xs
   _freeEngineList xs
   return xs'

foreign import ccall "evas_render_method_list" _listEngines :: IO (EinaList CString)

foreign import ccall "evas_render_method_list_free" _freeEngineList :: (EinaList CString) -> IO ()

-- | Set the output engine for the given canvas
foreign import ccall "evas_output_method_set" setCanvasEngineById :: Canvas -> Int -> IO ()

-- | Get the output engine id for the given canvas
foreign import ccall "evas_output_method_get" getCanvasEngineId :: Canvas -> IO Int

-- | Retrieves the current render engine info struct from the given canvas
foreign import ccall "evas_engine_info_get" getCanvasEngineInfo :: Canvas -> IO EngineInfo

-- | Get the output engine name for the given canvas
getCanvasEngineName :: Canvas -> IO String
getCanvasEngineName canvas = do
   n <- getCanvasEngineId canvas
   es <- listEngines
   return (es !! n)
