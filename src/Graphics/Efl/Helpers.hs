module Graphics.Efl.Helpers where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

size_get_helper :: (Ptr Int -> Ptr Int -> IO ()) -> IO (Int,Int)
size_get_helper f = do
  alloca $ \w ->
    alloca $ \h -> do
      f w h
      rw <- peek w
      rh <- peek h
      return (rw,rh)

geometry_get_helper :: (Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()) -> IO (Int,Int,Int,Int)
geometry_get_helper f = do
  alloca $ \w ->
    alloca $ \h ->
      alloca $ \x ->
        alloca $ \y -> do
          f x y w h
          rx <- peek x
          ry <- peek y
          rw <- peek w
          rh <- peek h
          return (rx,ry,rw,rh)

-- | Wrap a pointer in Maybe where nullPtr implies Nothing
maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr p | p == nullPtr = Nothing
maybePtr p = Just p
