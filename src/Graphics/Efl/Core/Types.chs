{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Efl.Core.Types (
   Animator, AnimatorSource,
   CoreCallback, CoreCallbackP, wrapCoreCallback
) where

import Foreign.Ptr

type Animator = Ptr ()
type AnimatorSource = Ptr ()

type CoreCallback = Ptr () -> IO ()
type CoreCallbackP = FunPtr CoreCallback

foreign import ccall "wrapper" wrapCoreCallback :: CoreCallback -> IO CoreCallbackP
