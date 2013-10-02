{-# Language ForeignFunctionInterface #-}

-- | EFL Core
module Graphics.Efl.Core (
   beginMainLoop, quitMainLoop
) where

#include <Ecore.h>

-- | Starts the main loop
foreign import ccall "ecore_main_loop_begin" beginMainLoop :: IO ()

-- | Quit the main loop
foreign import ccall "ecore_main_loop_quit" quitMainLoop :: IO ()
