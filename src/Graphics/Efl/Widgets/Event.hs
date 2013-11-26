module Graphics.Efl.Widgets.Event where

import Control.Applicative
import Data.IORef
import Data.Foldable
import Data.IntMap as IntMap


newtype Event = Event (IORef (IntMap (IO ())))

newtype Callback = Callback (Event,Int)

newEvent :: IO Event
newEvent = Event <$> newIORef IntMap.empty

addCallback :: IO () -> Event -> IO Callback
addCallback cb ev@(Event cbs) = do
   cbs' <- readIORef cbs
   let key = if IntMap.null cbs' 
                  then 0 
                  else fst (IntMap.findMax cbs') + 1
   modifyIORef cbs (insert key cb)
   return $ Callback (ev,key)

deleteCallback :: Callback -> IO ()
deleteCallback (Callback (Event cbs,key)) = modifyIORef cbs (delete key)

updateCallback :: Callback -> IO () -> IO ()
updateCallback (Callback (Event cbs,key)) cb = modifyIORef cbs (insert key cb)

triggerEvent :: Event -> IO ()
triggerEvent (Event cbs) = traverse_ id =<< (elems <$> readIORef cbs)


