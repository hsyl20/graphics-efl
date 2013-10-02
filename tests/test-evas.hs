module Main where

import System.Exit (exitFailure, exitSuccess)

import Control.Monad
import Control.Applicative
import Foreign.Ptr

import Graphics.Efl.Core
import Graphics.Efl.Window
import Graphics.Efl.Canvas
import Graphics.Efl.Canvas.Rectangle
import Graphics.Efl.Eina

assert :: String -> Bool -> IO ()
assert s b =
   if b then
      putStrLn $ s ++ ": PASS"
   else do
      putStrLn $ s ++ ": FAILED"
      exitFailure

assertM :: String -> IO Bool -> IO ()
assertM s b = assert s =<< b

main = do
   initWindowingSystem
   win <- createWindow Nothing 0 0 800 600 Nothing
   canvas <- getWindowCanvas win

   check_object_clipping canvas
   check_object_focus canvas
   check_object_layer canvas
   check_object_name canvas
   check_object_ref canvas
   check_object_size canvas
   check_object_color canvas
   check_object_evas canvas

   exitSuccess


check_object_clipping canvas = do
   r1 <- addRectangle canvas
   r2 <- addRectangle canvas
   r3 <- addRectangle canvas

   assertM "No initial clipping object" $ 
      (== Nothing) <$> getClippingObject r1

   assertM "Clipping object get . set == id" $ do
      setClippingObject r1 r2
      (== Just r2) <$> getClippingObject r1

   assertM "Clipping object unset" $ do
      setClippingObject r1 r2
      disableClipping r1
      (== Nothing) <$> getClippingObject r1

   assertM "Clipping object list get" $ do
      setClippingObject r1 r3
      setClippingObject r2 r3
      (== [r1,r2]) <$> getClipees r3


check_object_focus canvas = do
   r1 <- addRectangle canvas
   r2 <- addRectangle canvas

   assertM "Focus object get . set == id" $ do
      setFocus r2 True
      (== True) <$> isFocused r2


check_object_layer canvas = do
   r1 <- addRectangle canvas
   r2 <- addRectangle canvas

   assertM "Layer object get . set == id" $ do
      setLayer r1 10
      (== 10) <$> getLayer r1

   assertM "Layer object above" $ do
      setLayer r1 10
      setLayer r2 10
      stackAbove r1 r2
      (== Just r1) <$> getObjectAbove r2

   assertM "Layer object below" $ do
      setLayer r1 10
      setLayer r2 10
      stackBelow r1 r2
      (== Just r1) <$> getObjectBelow r2


check_object_name canvas = do
   r1 <- addRectangle canvas

   assertM "Name object get . set == id" $ do
      let name = "whatever"
      setName r1 name
      (== name) <$> getName r1

check_object_ref canvas = do
   r1 <- addRectangle canvas

   assertM "Ref object (-1) . (+1) == id" $ do
      t0 <- getRefCount r1
      retain r1
      t1 <- getRefCount r1
      release r1
      t2 <- getRefCount r1
      return (t2 == t0 && t1 == t0 + 1)

check_object_size canvas = do
   r1 <- addRectangle canvas

   assertM "Moving object" $ do
      (x0,y0,w0,h0) <- getGeometry r1
      let (x1,y1,w1,h1) = (x0+100, y0+150, w0+200, h0+250)
      resize r1 w1 h1
      move r1 x1 y1
      (x2,y2,w2,h2) <- getGeometry r1
      return (x1 == x2 && y1 == y2 && w1 == w2 && h1 == h2)


check_object_color canvas = do
   rect1 <- addRectangle canvas

   assertM "Coloring object" $ do
      let (r0,g0,b0,a0) = (10,20,30,40)
      setColor rect1 r0 g0 b0 a0
      (r1,g1,b1,a1) <- getColor rect1
      return (r0 == r1 && g0 == g1 && b0 == b1 && a0 == a1)


check_object_evas canvas = do
   r1 <- addRectangle canvas

   assertM "Retrieve object canvas" $ do
      (== canvas) <$> getCanvas r1
