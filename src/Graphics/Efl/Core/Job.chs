{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Efl.Core.Job (
   createJob, destroyJob
) where

import Foreign.Ptr
import Graphics.Efl.Core.Types

type Job = Ptr ()


-- | Create a job
createJob :: IO () -> IO Job
createJob f = flip _createJob nullPtr =<< wrapCoreCallback (const f)

foreign import ccall "ecore_job_add" _createJob :: CoreCallbackP -> Ptr () -> IO Job

-- | Destroy a job
foreign import ccall "ecore_job_del" destroyJob :: Job -> IO ()
