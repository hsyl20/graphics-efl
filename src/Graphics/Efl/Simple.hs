-- | Simple interface to create a window
module Graphics.Efl.Simple (
   module M,
   (|>),
   defaultWindow, defaultWindowEx, withDefaultWindow
) where

import Graphics.Efl.Window as M
import Graphics.Efl.Canvas as M
import Graphics.Efl.Core as M

import Control.Concurrent

-- | Initialize a default window. Use the MVar to block the main thread if necessary
defaultWindowEx :: Maybe String -> IO (Window,Canvas,MVar ())
defaultWindowEx engine = do
   initWindowingSystem
   w <- createWindow engine 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   v <- newEmptyMVar
   _ <- forkOS (beginMainLoop >> putMVar v ())
   return (w,c,v)

-- | Initialize a default window.
--The main thread must not be killed for it to continue to work
defaultWindow :: Maybe String -> IO (Window,Canvas)
defaultWindow engine = do
   initWindowingSystem
   w <- createWindow engine 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   _ <- forkOS beginMainLoop
   return (w,c)

-- | Create a default window and block
withDefaultWindow :: Maybe String -> (Window -> Canvas -> IO ()) -> IO ()
withDefaultWindow engine f = do
   initWindowingSystem
   w <- createWindow engine 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   _ <- forkIO (f w c)
   beginMainLoop
   destroyWindow w
   shutdownWindowingSystem


(|>) :: IO a -> (a -> IO ()) -> IO a
(|>) obj f = do
   o <- obj
   f o
   return o
