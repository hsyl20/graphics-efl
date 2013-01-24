module Graphics.Efl.Helpers where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

get2_helper :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO ()) -> IO (a,b)
get2_helper f = do
  alloca $ \w ->
    alloca $ \h -> do
      f w h
      rw <- peek w
      rh <- peek h
      return (rw,rh)

get4_helper :: (Storable a, Storable b, Storable c, Storable d) => (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO ()) -> IO (a,b,c,d)
get4_helper f = do
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
