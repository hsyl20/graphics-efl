module Graphics.Efl.Simple (
   module M,
   defaultWindow
) where

import Graphics.Efl.Window as M
import Graphics.Efl.Canvas as M
import Graphics.Efl.Core

import Control.Concurrent

-- | Initialize a default window
defaultWindow :: IO (Window,Canvas)
defaultWindow = do
   initWindowingSystem
   w <- createWindow Nothing 0 0 800 600 Nothing
   c <- getWindowCanvas w
   showWindow w
   _ <- forkIO beginMainLoop
   return (w,c)
