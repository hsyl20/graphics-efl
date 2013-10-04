-- | Simple interface to create a window
module Graphics.Efl.Simple (
   module M,
   defaultWindow, defaultWindowEx, withDefaultWindow
) where

import Graphics.Efl.Window as M
import Graphics.Efl.Canvas as M
import Graphics.Efl.Core

import Control.Concurrent

-- | Initialize a default window. Use the MVar to block the main thread if necessary
defaultWindowEx :: IO (Window,Canvas,MVar ())
defaultWindowEx = do
   initWindowingSystem
   w <- createWindow Nothing 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   v <- newEmptyMVar
   _ <- forkFinally beginMainLoop (\_ -> putMVar v ())
   return (w,c,v)

-- | Initialize a default window.
--The main thread must not be killed for it to continue to work
defaultWindow :: IO (Window,Canvas)
defaultWindow = do
   initWindowingSystem
   w <- createWindow Nothing 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   _ <- forkIO beginMainLoop
   return (w,c)

-- | Create a default window and block
withDefaultWindow :: (Window -> Canvas -> IO a) -> IO a
withDefaultWindow f = do
   (w,c,v) <- defaultWindowEx
   r <- f w c
   readMVar v
   return r
