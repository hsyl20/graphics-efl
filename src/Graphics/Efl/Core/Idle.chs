{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Efl.Core.Idle
   ( addIdleEnterer
   )
where

import Foreign.Ptr
import Graphics.Efl.Core.Types
import Graphics.Efl.Eina

type IdleEnterer = Ptr ()

foreign import ccall "ecore_idle_enterer_add" _idleEntererAdd :: TaskCallbackP -> Ptr () -> IO IdleEnterer

-- | Add an idle enterer
addIdleEnterer :: IO EinaBool -> IO IdleEnterer
addIdleEnterer f = flip _idleEntererAdd nullPtr =<< wrapTaskCallback (const f)
