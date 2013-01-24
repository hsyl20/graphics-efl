{-# Language ForeignFunctionInterface #-}

module Graphics.Efl.Eina where

import Foreign.Ptr
import Foreign.Storable

import Control.Applicative

#include <Eina.h>

-- | Double-linked list
type EinaList a = Ptr ()

-- | Next element in the list
list_next :: EinaList a -> IO (EinaList a)
list_next = {#get Eina_List->next#}

-- | Previous element in the list
list_prev :: EinaList a -> IO (EinaList a)
list_prev = {#get Eina_List->prev#}

-- | Data in the current cell
list_data_get :: EinaList a -> IO (Ptr a)
list_data_get el = castPtr <$> {#get Eina_List->data#} el

-- | Convert a Eina list into a list
toList :: EinaList a -> IO [Ptr a]
toList el | el == nullPtr = return []
toList el = do
   x <- list_data_get el
   xs <- toList =<< list_next el
   return (x:xs)
