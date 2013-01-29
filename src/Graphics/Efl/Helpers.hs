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

get2_ex_helper :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> IO (a,b,c)
get2_ex_helper f = do
  alloca $ \w ->
    alloca $ \h -> do
      c <- f w h
      rw <- peek w
      rh <- peek h
      return (rw,rh,c)

get3_helper :: (Storable a, Storable b, Storable c) => (Ptr a -> Ptr b -> Ptr c -> IO ()) -> IO (a,b,c)
get3_helper f = do
  alloca $ \a ->
    alloca $ \b ->
      alloca $ \c -> do
         f a b c
         ra <- peek a
         rb <- peek b
         rc <- peek c
         return (ra,rb,rc)

get3_ex_helper :: (Storable a, Storable b, Storable c) => (Ptr a -> Ptr b -> Ptr c -> IO e) -> IO (a,b,c,e)
get3_ex_helper f = do
  alloca $ \a ->
    alloca $ \b ->
      alloca $ \c -> do
         e <- f a b c
         ra <- peek a
         rb <- peek b
         rc <- peek c
         return (ra,rb,rc,e)

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

get4_ex_helper :: (Storable a, Storable b, Storable c, Storable d) => (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO e) -> IO (a,b,c,d,e)
get4_ex_helper f = do
  alloca $ \w ->
    alloca $ \h ->
      alloca $ \x ->
        alloca $ \y -> do
          e <- f x y w h
          rx <- peek x
          ry <- peek y
          rw <- peek w
          rh <- peek h
          return (rx,ry,rw,rh,e)

-- | Wrap a pointer in Maybe where nullPtr implies Nothing
maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr p | p == nullPtr = Nothing
maybePtr p = Just p
