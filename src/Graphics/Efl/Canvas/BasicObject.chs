{-# Language ForeignFunctionInterface #-}

-- | Methods for canvas objects
module Graphics.Efl.Canvas.BasicObject (
   setClippingObject, getClippingObject,
   disableClipping, getClipees,
   setFocus, isFocused,
   setLayer, getLayer,
   setName, getName,
   retain, release, getRefCount,
   delete,
   move, resize, getGeometry,
   cover, uncover, isVisible,
   getCanvas, getType,
   raise, lower, stackAbove, stackBelow,
   getObjectBelow, getObjectAbove,
   setObjectColor, getObjectColor,
   setObjectSize, getObjectSize,
   setObjectPosition, getObjectPosition,
   setObjectVisible, getObjectVisible,
   setObjectFocus, getObjectFocus
) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Applicative
import Data.Int

import Graphics.Efl.Helpers
import Graphics.Efl.Eina
import Graphics.Efl.Canvas.Types


-- | Clip one object to another
setClippingObject :: Object -> Object -> IO ()
setClippingObject = flip _setClippingObject

foreign import ccall "evas_object_clip_set" _setClippingObject :: Object -> Object -> IO ()

-- | Get the object clipping the given object (if any)
getClippingObject :: Object -> IO (Maybe Object)
getClippingObject obj = maybePtr <$> _object_clip_get obj

foreign import ccall "evas_object_clip_get" _object_clip_get :: Object -> IO Object
   
-- | Disable/cease clipping on a clipped object
foreign import ccall "evas_object_clip_unset" disableClipping :: Object -> IO ()

-- | Return a list of objects currently clipped by obj
getClipees :: Object -> IO [Object]
getClipees obj = toList =<< _object_clipees_get obj

foreign import ccall "evas_object_clipees_get" _object_clipees_get :: Object -> IO (EinaList Object)

-- | Set object focus attribute
setObjectFocus :: Bool -> Object -> IO ()
setObjectFocus = setFocus

-- | Get object focus attribute
getObjectFocus :: Object -> IO Bool
getObjectFocus = isFocused

-- | Set or unset a given object as the currently focused one on its canvas
setFocus :: Bool -> Object -> IO ()
setFocus True obj = _object_focus_set obj 1
setFocus False obj = _object_focus_set obj 0

foreign import ccall "evas_object_focus_set" _object_focus_set :: Object -> EinaBool -> IO ()

-- | Retrieve whether an object has the focus 
isFocused :: Object -> IO Bool
isFocused obj = (/= 0) <$> _object_focus_get obj

foreign import ccall "evas_object_focus_get" _object_focus_get :: Object -> IO EinaBool



-- | Sets the layer of its canvas that the given object will be part of
setLayer :: Int16 -> Object -> IO ()
setLayer n o = _setLayer o (fromIntegral n)

foreign import ccall "evas_object_layer_set" _setLayer :: Object -> CShort -> IO ()

-- | Retrieves the layer of its canvas that the given object is part of
getLayer :: Object -> IO Int16
getLayer o = fromIntegral <$> _getLayer o

foreign import ccall "evas_object_layer_get" _getLayer :: Object -> IO CShort



-- | Set the name of the given Evas object to the given name
setName :: String -> Object -> IO ()
setName name obj = withCString name (_object_name_set obj)

foreign import ccall "evas_object_name_set" _object_name_set :: Object -> CString -> IO ()

-- | Retrieve the name of the given Evas object
getName :: Object -> IO String
getName obj = do
   ptr <- _object_name_get obj
   if ptr == nullPtr then return "" else peekCString ptr

foreign import ccall "evas_object_name_get" _object_name_get :: Object -> IO CString



-- | Increment object reference count to defer its deletion
foreign import ccall "evas_object_ref" retain :: Object -> IO ()

-- | Decrement object reference count
foreign import ccall "evas_object_unref" release :: Object -> IO ()

-- | Get the object reference count
foreign import ccall "evas_object_ref_get" getRefCount :: Object -> IO CInt



-- | Mark the given Evas object for deletion (when Evas will free its memory)
foreign import ccall "evas_object_del" delete :: Object -> IO ()



-- | Move the given Evas object to the given location inside its canvas' viewport
move :: Int -> Int -> Object -> IO ()
move x y obj = _move obj (fromIntegral x) (fromIntegral y)

foreign import ccall "evas_object_move" _move :: Object -> Coord -> Coord -> IO ()

-- | Change the size of the given Evas object
resize :: Int -> Int -> Object -> IO ()
resize w h obj = _resize obj (fromIntegral w) (fromIntegral h)

foreign import ccall "evas_object_resize" _resize :: Object -> Coord -> Coord -> IO ()


-- | Get object size
getObjectSize :: Object -> IO (Int,Int)
getObjectSize obj = do
   (_,_,w,h) <- getGeometry obj
   return (w,h)

-- | Set object size
setObjectSize :: (Int,Int) -> Object -> IO ()
setObjectSize (w,h) = resize w h

-- | Get object position
getObjectPosition :: Object -> IO (Int,Int)
getObjectPosition obj = do
   (x,y,_,_) <- getGeometry obj
   return (x,y)

-- | Set object position
setObjectPosition :: (Int,Int) -> Object -> IO ()
setObjectPosition (x,y) = move x y

-- | Retrieve the position and (rectangular) size of the given Evas object
getGeometry :: Object -> IO (Int,Int,Int,Int)
getGeometry obj = do
   (a,b,c,d) <- get4_helper (_object_geometry_get obj)
   return (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

foreign import ccall "evas_object_geometry_get" _object_geometry_get :: Object -> Ptr Coord -> Ptr Coord -> Ptr Coord -> Ptr Coord -> IO ()


-- | Set object visible attribute
setObjectVisible :: Bool -> Object -> IO ()
setObjectVisible True = uncover
setObjectVisible False = cover

-- | Get object visible attribute
getObjectVisible :: Object -> IO Bool
getObjectVisible = isVisible

-- | Make the given Evas object visible
foreign import ccall "evas_object_show" uncover :: Object -> IO ()

-- | Make the given Evas object invisible
foreign import ccall "evas_object_hide" cover :: Object -> IO ()

-- | Retrieve whether or not the given Evas object is visible
isVisible :: Object -> IO Bool
isVisible obj = toBool <$> object_visible_get_ obj

foreign import ccall "evas_object_visible_get" object_visible_get_ :: Object -> IO EinaBool



-- | Set the general/main color of the given Evas object to the given one
setObjectColor :: (Int,Int,Int,Int) -> Object -> IO ()
setObjectColor (r,g,b,a) obj = _setColor obj (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

foreign import ccall "evas_object_color_set" _setColor :: Object -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Set the general/main color of the given Evas object to the given one
getObjectColor :: Object -> IO (Int,Int,Int,Int)
getObjectColor obj = f <$> get4_helper (_object_color_get obj)
   where f (r,g,b,a) = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)

foreign import ccall "evas_object_color_get" _object_color_get :: Object -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()


-- | Retrieve the Evas canvas that the given object lives on
foreign import ccall "evas_object_evas_get" getCanvas :: Object -> IO Canvas


-- | Retrieve the type of the given Evas object
getType :: Object -> IO String
getType obj = peekCString =<< _object_type_get obj

foreign import ccall "evas_object_type_get" _object_type_get :: Object -> IO CString



-- | Raise object to the top of its layer
foreign import ccall "evas_object_raise" raise :: Object -> IO ()

-- | Lower object to the bottom of its layer
foreign import ccall "evas_object_lower" lower :: Object -> IO ()

-- | Stack an object immediately above another object
stackAbove :: Object -> Object -> IO ()
stackAbove = flip _stackAbove

foreign import ccall "evas_object_stack_above" _stackAbove :: Object -> Object -> IO ()

-- | Stack an object immediately below another object
stackBelow :: Object -> Object -> IO ()
stackBelow = flip _stackBelow

foreign import ccall "evas_object_stack_below" _stackBelow :: Object -> Object -> IO ()

-- | Get the Evas object stacked right above an object
getObjectAbove :: Object -> IO (Maybe Object)
getObjectAbove obj = maybePtr <$> _object_above_get obj

foreign import ccall "evas_object_above_get" _object_above_get :: Object -> IO Object

-- | Get the Evas object stacked right below an object
getObjectBelow :: Object -> IO (Maybe Object)
getObjectBelow obj = maybePtr <$> _object_below_get obj

foreign import ccall "evas_object_below_get" _object_below_get :: Object -> IO Object
