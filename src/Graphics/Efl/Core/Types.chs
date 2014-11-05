{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Efl.Core.Types
   ( Animator
   , AnimatorSource
   , CoreCallback
   , CoreCallbackP
   , wrapCoreCallback
   , TaskCallback
   , TaskCallbackP
   , wrapTaskCallback
   )
where

import Foreign.Ptr
import Foreign.C.Types

type Animator = Ptr ()
type AnimatorSource = Ptr ()

type CoreCallback = Ptr () -> IO ()
type CoreCallbackP = FunPtr CoreCallback

foreign import ccall "wrapper" wrapCoreCallback :: CoreCallback -> IO CoreCallbackP

type TaskCallback = Ptr () -> IO CUChar -- should be EinaBool but c2hs doesn't recognize the type alias
type TaskCallbackP = FunPtr TaskCallback

foreign import ccall "wrapper" wrapTaskCallback :: TaskCallback -> IO TaskCallbackP
