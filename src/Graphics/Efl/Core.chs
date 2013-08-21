{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Core where

import Foreign.Ptr

#include <Ecore.h>

type EcoreFdHandler = Ptr ()
type EcoreFdCb = FunPtr (Ptr () -> EcoreFdHandler -> IO ())

fdRead, fdWrite, fdError :: Int
fdRead = 1
fdWrite = 2
fdError = 4 

foreign import ccall "ecore_main_loop_begin" beginMainLoop :: IO ()
foreign import ccall "ecore_main_loop_quit" quitMainLoop :: IO ()

foreign import ccall "ecore_main_fd_handler_add" addMainFdHandler :: Int -> Int -> EcoreFdCb -> Ptr () -> EcoreFdCb -> Ptr () -> IO ()

foreign import ccall "wrapper" wrapCallback :: (Ptr () -> EcoreFdHandler -> IO ()) -> IO EcoreFdCb
