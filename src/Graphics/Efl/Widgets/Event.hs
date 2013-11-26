module Graphics.Efl.Widgets.Event where

import Control.Applicative
import Data.IORef
import Data.Foldable

data Event = Event {
   callbacks :: IORef [IO ()]
}

newEvent :: IO Event
newEvent = Event <$> newIORef []

addCallback :: IO () -> Event -> IO ()
addCallback cb ev = modifyIORef (callbacks ev) (cb:) 

triggerEvent :: Event -> IO ()
triggerEvent ev = traverse_ id =<< readIORef (callbacks ev)


