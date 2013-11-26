{-# LANGUAGE TupleSections, ExistentialQuantification #-}
module Graphics.Efl.Widgets.Reactive where

import Graphics.Efl.Widgets.Event

import Control.Applicative
import Data.IORef
import Control.Monad (forM_,void)
import Control.Monad.IO.Class
import Data.Maybe

data Property a = Property {
   setValue :: a -> IO (),       -- ^ Property internal setter
   getValue :: IO a,             -- ^ Property internal getter
   propEvent :: Event,           -- ^ Event triggered when the property is set
   propSources :: IORef [Event]  -- ^ Events that trigger reevaluation of the property
}

newProperty :: (a -> IO ()) -> IO a -> IO (Property a)
newProperty wr rd = Property wr rd <$> newEvent <*> newIORef []

newIORefProperty :: a -> IO (Property a)
newIORefProperty a = do
   v <- newIORef a
   newProperty (writeIORef v) (readIORef v)


-- | Binding
-- * List of events that are depended upon 
-- * Value
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
triggerPropertyEvent prop = triggerEvent (propEvent prop)

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

(=&) :: Property a -> Binding a -> IO ()
(=&) prop (Binding f) = do
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


data Signal a = Signal Event (IORef (Maybe a))

newSignal :: IO (Signal a)
newSignal = Signal <$> newEvent <*> newIORef Nothing

triggerSignal :: Signal a -> a -> IO ()
triggerSignal (Signal ev ref) v = do
   writeIORef ref (Just v)
   triggerEvent ev
   writeIORef ref Nothing


data Transition s = forall a . Transition (Signal a) (a -> Auto s)

data Auto s = Auto s [Transition s]

runAutomaton :: Auto s -> (s -> a) -> IO (Property a)
runAutomaton (Auto initState ts) f = do

   prop <- newIORefProperty (f initState)

   let runAutomaton' trs = 
         forM_ trs $ \(Transition (Signal ev ref) g) -> flip addCallback ev $ do
            Auto state' trs' <- g . fromJust <$> readIORef ref
            writeProperty prop (f state')
            --FIXME: remove old transition callbacks
            -- forM_ trs $ \(Transition (Signal ev _) _) -> removeCallback ev
            runAutomaton' trs'
   
   runAutomaton' ts

   return prop