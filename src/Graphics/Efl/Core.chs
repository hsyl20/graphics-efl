{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Core where

import Foreign.Ptr

#include <Ecore.h>

type EcoreFdHandler = Ptr ()
type EcoreFdCb = FunPtr (Ptr () -> EcoreFdHandler -> IO ())

ecore_fd_read = 1
ecore_fd_write = 2
ecore_fd_error = 4 

foreign import ccall "ecore_main_loop_begin" ecore_main_loop_begin :: IO ()
foreign import ccall "ecore_main_loop_quit" ecore_main_loop_quit :: IO ()

foreign import ccall "ecore_main_fd_handler_add" ecore_main_fd_handler_add :: Int -> Int -> EcoreFdCb -> Ptr () -> EcoreFdCb -> Ptr () -> IO ()

foreign import ccall "wrapper" ecore_fd_wrap_callback :: (Ptr () -> EcoreFdHandler -> IO ()) -> IO EcoreFdCb
