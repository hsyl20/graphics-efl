{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Core.Animator (
   addAnimator, addTimedAnimator, destroyAnimator,
   freezeAnimator, thawAnimator,
   setAnimatorFrametime, getAnimatorFrametime,
   setAnimatorFrameRate, getAnimatorFrameRate,
   mapAnimatorPos, setAnimatorSource, getAnimatorSource,
   setAnimatorCustomSourceTickBeginCallback,
   setAnimatorCustomSourceTickEndCallback,
   tickAnimatorCustom,
   addAnimationI, addAnimationLinearI, addAnimationBounceI, addAnimationSinusoidalI,
   addAnimation, addAnimationLinear, addAnimationBounce, addAnimationSinusoidal,
   addAnimationLinearI', addAnimationBounceI', addAnimationSinusoidalI',
   addAnimationLinear', addAnimationBounce', addAnimationSinusoidal'
) where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad

import Data.Fixed (mod')
import System.Time.Monotonic
import Graphics.Efl.Eina
import Graphics.Efl.Core.Types

type AnimationCb = Ptr () -> Double -> IO EinaBool
type AnimationCbP = FunPtr AnimationCb

wrapCallback' :: (Double -> IO Bool) -> IO AnimationCbP
wrapCallback' cb = wrapCallback (\ _ pos -> fromBool <$> cb pos)

foreign import ccall "wrapper" wrapCallback :: AnimationCb -> IO AnimationCbP


-- | Add a new animation
addAnimation :: (Double -> Double) -> Double -> Maybe Int -> (Double -> IO ()) -> IO Animator
addAnimation  modif period reps f = addAnimationI modif period reps (\x -> f x >> return True)

-- | Add a new interruptible animation
addAnimationI :: (Double -> Double) -> Double -> Maybe Int -> (Double -> IO Bool) -> IO Animator
addAnimationI modif period (Just r) f = do

   let normalize :: Double -> Double
       normalize 1.0 = 1.0
       normalize t = (/period) . (`mod'` period) . (* (fromIntegral r)) . (* period) $ t
       g = f . modif . normalize

   addTimedAnimator (period * fromIntegral r) g

addAnimationI modif period Nothing f = do
   clk <- newClock
   t0 <- clockGetTime clk

   let g :: Double -> IO Double
       g _ = modif . (/period) . (`mod'` period) . realToFrac . (t0-) <$> clockGetTime clk

   addAnimator (f <=< g)

-- | Add an animation with linear steps
addAnimationLinear :: Double -> Maybe Int -> (Double -> IO ()) -> IO Animator
addAnimationLinear = addAnimation id

-- | Add an interruptible animation with linear steps
addAnimationLinearI :: Double -> Maybe Int -> (Double -> IO Bool) -> IO Animator
addAnimationLinearI = addAnimationI id

-- | Add an animation with linear steps
addAnimationLinear' :: Double -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationLinear' period reps f = void (addAnimationLinear period reps f)

-- | Add an interruptible animation with linear steps
addAnimationLinearI' :: Double -> Maybe Int -> (Double -> IO Bool) -> IO ()
addAnimationLinearI' period reps f = void (addAnimationLinearI period reps f)

-- | Add an animation with bouncing steps
addAnimationBounce :: Double -> Maybe Int -> (Double -> IO ()) -> IO Animator
addAnimationBounce = addAnimation bounce

-- | Add an interruptible animation with bouncing steps
addAnimationBounceI :: Double -> Maybe Int -> (Double -> IO Bool) -> IO Animator
addAnimationBounceI = addAnimationI bounce

-- | Add an animation with bouncing steps
addAnimationBounce' :: Double -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationBounce' period reps f = void (addAnimationBounce period reps f)

-- | Add an interruptible animation with bouncing steps
addAnimationBounceI' :: Double -> Maybe Int -> (Double -> IO Bool) -> IO ()
addAnimationBounceI' period reps f = void (addAnimationBounceI period reps f)

bounce :: Double -> Double
bounce x | x <= 0.5  = 2.0 * x
         | otherwise = 2.0 * (1.0 - x)

-- | Add an interruptible animation with sinusoidal steps
addAnimationSinusoidalI :: Int -> Double -> Maybe Int -> (Double -> IO Bool) -> IO Animator
addAnimationSinusoidalI periods = addAnimationI (sino periods)

-- | Add an animation with sinusoidal steps
addAnimationSinusoidal :: Int -> Double -> Maybe Int -> (Double -> IO ()) -> IO Animator
addAnimationSinusoidal periods = addAnimation (sino periods)

-- | Add an interruptible animation with sinusoidal steps
addAnimationSinusoidalI' :: Int -> Double -> Maybe Int -> (Double -> IO Bool) -> IO ()
addAnimationSinusoidalI' periods period reps f = void (addAnimationSinusoidalI periods period reps f)

-- | Add an animation with sinusoidal steps
addAnimationSinusoidal' :: Int -> Double -> Maybe Int -> (Double -> IO ()) -> IO ()
addAnimationSinusoidal' periods period reps f = void (addAnimationSinusoidal periods period reps f)

sino :: Int -> Double -> Double
sino periods x = abs . sin $ 2.0 * pi * p' * x
   where p' = fromIntegral periods :: Double


-- | Add an animator to call  func at every animation tick during main
addAnimator :: (Double -> IO Bool) -> IO Animator
addAnimator cb = flip _addAnimator nullPtr =<< wrapCallback' cb

foreign import ccall "ecore_animator_add" _addAnimator :: AnimationCbP -> Ptr () -> IO Animator

-- | Add a animator that runs for a limited time
addTimedAnimator :: Double -> (Double -> IO Bool) -> IO Animator
addTimedAnimator d cb = flip (_addTimedAnimator d) nullPtr =<< wrapCallback' cb

foreign import ccall "ecore_animator_timeline_add" _addTimedAnimator :: Double -> AnimationCbP -> Ptr () -> IO Animator

-- | Delete the specified animator from the animator list
foreign import ccall "ecore_animator_del" destroyAnimator :: Animator -> IO ()

-- | Suspend the specified animator
foreign import ccall "ecore_animator_freeze" freezeAnimator :: Animator -> IO ()

-- | Restore execution of the specified animator
foreign import ccall "ecore_animator_thaw" thawAnimator :: Animator -> IO ()

-- | Set the animator call interval in seconds
foreign import ccall "ecore_animator_frametime_set" setAnimatorFrametime :: Double -> IO ()

-- | Set the animator frame rate
setAnimatorFrameRate :: Int -> IO ()
setAnimatorFrameRate n = setAnimatorFrametime (1.0 / (fromIntegral n :: Double))

-- | Get the animator call interval in seconds
foreign import ccall "ecore_animator_frametime_get" getAnimatorFrametime :: IO Double

-- | Get the animator frame rate
getAnimatorFrameRate :: IO Int
getAnimatorFrameRate = round . (1.0 /) <$> getAnimatorFrametime

-- | Maps an input position from 0.0 to 1.0 along a timeline to a position in a different curve
foreign import ccall "ecore_animator_pos_map" mapAnimatorPos :: Double -> CInt -> Double -> Double -> IO Double

-- | Set the source of animator ticks for the mainloop
foreign import ccall "ecore_animator_source_set" setAnimatorSource :: AnimatorSource -> IO ()

-- | Get the animator source currently set
foreign import ccall "ecore_animator_source_get" getAnimatorSource :: IO AnimatorSource

-- | Set the function that begins a custom animator tick source
foreign import ccall "ecore_animator_custom_source_tick_begin_callback_set" setAnimatorCustomSourceTickBeginCallback :: CoreCallbackP -> Ptr () -> IO ()

-- | Set the function that ends a custom animator tick source
foreign import ccall "ecore_animator_custom_source_tick_end_callback_set" setAnimatorCustomSourceTickEndCallback :: CoreCallbackP -> Ptr () -> IO ()

-- | Trigger a custom animator tick
foreign import ccall "ecore_animator_custom_tick" tickAnimatorCustom :: IO ()
