-- | Animators
module Graphics.Efl.Animator (
   Animator,
   createAnimator, setAnimatorPeriod, setAnimatorFrameRate,
   addAnimation, seconds,
   addAnimationLinear, addAnimationBounce, addAnimationSinusoidal
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (filterM)

import Data.Maybe
import Data.Fixed (divMod')
import Data.Time.Clock
import System.Time.Monotonic

-- | An animator
data Animator = Animator {
   clock :: Clock,                -- Clock
   period :: TVar DiffTime,       -- frame time
   animations :: TVar [Animation]
}

-- | An animation
data Animation = Animation {
   beginDate :: DiffTime,
   duration :: DiffTime,
   modifier :: Double -> Double,
   repeated :: Maybe Int,
   callback :: Double -> IO ()
}

-- | Create a new animator
createAnimator :: IO Animator
createAnimator = do
   ftime <- newTVarIO (50 * 1000 * 1000)
   clk <- newClock
   anims <- newTVarIO []
   let anim = Animator clk ftime anims
   _ <- forkIO (animatorThread anim)
   return anim

animatorThread :: Animator -> IO ()
animatorThread anim = do
   t0 <- clockGetTime (clock anim)

   -- Evaluate animations
   (priod, anims) <- atomically $ do
      p <- readTVar (period anim)
      as <- readTVar (animations anim)
      writeTVar (animations anim) []
      return (p,as)

   let evalAnim a = do
         let (rep,modul) = t1 `divMod'` duration a
             step = modul / duration a
             step' = (fromRational (toRational step))
             t1 = t0 - beginDate a
         if isJust (repeated a) && fromJust (repeated a) <= rep 
            then do
               callback a (modifier a 1.0)
               return False
            else do
               callback a (modifier a step')
               return True

   anims' <- filterM evalAnim anims
         
   atomically $ modifyTVar (animations anim) (anims'++)

   -- Wait for the remaining frame time
   t1 <- clockGetTime (clock anim)
   let t3 = max 0 (priod - (t1-t0))

   delay t3

   animatorThread anim


-- | Set the animator call interval in seconds
setAnimatorPeriod :: DiffTime -> Animator -> IO ()
setAnimatorPeriod ts anim = atomically $ writeTVar (period anim) ts

-- | Set the number of ticks per second
setAnimatorFrameRate :: Int -> Animator -> IO ()
setAnimatorFrameRate n = setAnimatorPeriod (seconds p)
   where p = 1.0 / (fromIntegral n :: Double)

-- | Convert seconds to DiffTime
seconds :: Double -> DiffTime
seconds = picosecondsToDiffTime . floor . (* 1e12)


-- | Add a new animation
addAnimation :: Animator -> DiffTime -> Maybe Int -> (Double -> Double) -> (Double -> IO ()) -> IO ()
addAnimation ator dur reps modif f = do
   t0 <- clockGetTime (clock ator)
   let anim = Animation t0 dur modif reps f
   atomically $ modifyTVar (animations ator) (anim:)

-- | Add an animation with linear steps
addAnimationLinear :: Animator -> DiffTime -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationLinear ator dur reps f = addAnimation ator dur reps id f

-- | Add an animation with bouncing steps
addAnimationBounce :: Animator -> DiffTime -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationBounce ator dur reps f = addAnimation ator dur reps g f
   where g x | x <= 0.5  = 2.0 * x
             | otherwise = 2.0 * (1.0 - x)

-- | Add an animation with sinusoidal steps
addAnimationSinusoidal :: Animator -> Int -> DiffTime -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationSinusoidal ator periods dur reps f = addAnimation ator dur reps g f
   where g x = abs . sin $ 2.0 * pi * p' * x
         p' = fromIntegral periods :: Double
