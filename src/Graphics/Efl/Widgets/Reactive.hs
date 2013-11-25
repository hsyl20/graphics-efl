{-# LANGUAGE TupleSections #-}
module Graphics.Efl.Widgets.Reactive where

import Control.Applicative
import Data.IORef
import Control.Monad (forM_,void)
import Control.Monad.IO.Class
import Data.Foldable(traverse_)

data Event = Event {
   callbacks :: IORef [IO ()]
}

newEvent :: IO Event
newEvent = Event <$> newIORef []


data Property a = Property {
   setValue :: a -> IO (),
   getValue :: IO a,
   propEvent :: Event,
   propSources :: IORef [Event]
}

newProperty :: (a -> IO ()) -> IO a -> IO (Property a)
newProperty wr rd = Property wr rd <$> newEvent <*> newIORef []

newIORefProperty :: a -> IO (Property a)
newIORefProperty a = do
   v <- newIORef a
   newProperty (writeIORef v) (readIORef v)


data Binding a = Binding (IO ([Event],a))

readProperty :: Property a -> Binding a
readProperty prop = Binding $ do
   val <- getValue prop
   return ([propEvent prop], val)

writeProperty :: Property a -> a -> IO ()
writeProperty prop val = do
   setValue prop val
   triggerPropertyEvent prop

triggerPropertyEvent :: Property a -> IO ()
triggerPropertyEvent prop = traverse_ id =<< readIORef (callbacks (propEvent prop))

instance Functor Binding where
   fmap f (Binding g) = Binding $ do
                           (evs,val) <- g
                           return (evs, f val)

instance Monad Binding where
   return a = Binding (return ([],a))
   (Binding f) >>= g = Binding $ do
                           (eva,a) <- f
                           let Binding g' = g a
                           (evb,b) <- g'
                           return (eva++evb, b)

instance MonadIO Binding where
   liftIO f = Binding $ (([],) <$> f)

(<--) :: Property a -> Binding a -> IO ()
(<--) prop (Binding f) = do
   let cb = do
         (evs,val) <- f
         -- set sources
         writeIORef (propSources prop) evs
         -- set value
         writeProperty prop val
         return evs

   -- Set initial value
   evs <- cb

   -- Assign callback
   forM_ evs (addCallback (void cb))


addCallback :: IO () -> Event -> IO ()
addCallback cb ev = modifyIORef (callbacks ev) (cb:) 
