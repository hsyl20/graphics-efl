module Main where

import System.Exit (exitFailure, exitSuccess)

import Control.Monad
import Control.Applicative
import Foreign.Ptr

import Graphics.Efl.Ecore
import Graphics.Efl.EcoreEvas
import Graphics.Efl.Evas
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
   ecore_evas_init
   ee <- ecore_evas_new nullPtr 0 0 800 600 nullPtr
   canvas <- ecore_evas_get ee

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
   r1 <- evas_object_rectangle_add canvas
   r2 <- evas_object_rectangle_add canvas
   r3 <- evas_object_rectangle_add canvas

   assertM "No initial clipping object" $ 
      (== Nothing) <$> object_clip_get r1

   assertM "Clipping object get . set == id" $ do
      object_clip_set r1 r2
      (== Just r2) <$> object_clip_get r1

   assertM "Clipping object unset" $ do
      object_clip_set r1 r2
      object_clip_unset r1
      (== Nothing) <$> object_clip_get r1

   assertM "Clipping object list get" $ do
      object_clip_set r1 r3
      object_clip_set r2 r3
      (== [r1,r2]) <$> object_clipees_get r3


check_object_focus canvas = do
   r1 <- evas_object_rectangle_add canvas
   r2 <- evas_object_rectangle_add canvas

   assertM "Focus object get . set == id" $ do
      object_focus_set r2 True
      (== True) <$> object_focus_get r2


check_object_layer canvas = do
   r1 <- evas_object_rectangle_add canvas
   r2 <- evas_object_rectangle_add canvas

   assertM "Layer object get . set == id" $ do
      object_layer_set r1 10
      (== 10) <$> object_layer_get r1

   assertM "Layer object above" $ do
      object_layer_set r1 10
      object_layer_set r2 10
      object_stack_above r1 r2
      (== Just r1) <$> object_above_get r2

   assertM "Layer object below" $ do
      object_layer_set r1 10
      object_layer_set r2 10
      object_stack_below r1 r2
      (== Just r1) <$> object_below_get r2


check_object_name canvas = do
   r1 <- evas_object_rectangle_add canvas

   assertM "Name object get . set == id" $ do
      let name = "whatever"
      object_name_set r1 name
      (== name) <$> object_name_get r1

check_object_ref canvas = do
   r1 <- evas_object_rectangle_add canvas

   assertM "Ref object (-1) . (+1) == id" $ do
      t0 <- object_ref_get r1
      object_ref r1
      t1 <- object_ref_get r1
      object_unref r1
      t2 <- object_ref_get r1
      return (t2 == t0 && t1 == t0 + 1)

check_object_size canvas = do
   r1 <- evas_object_rectangle_add canvas

   assertM "Moving object" $ do
      (x0,y0,w0,h0) <- object_geometry_get r1
      let (x1,y1,w1,h1) = (x0+100, y0+150, w0+200, h0+250)
      object_resize r1 w1 h1
      object_move r1 x1 y1
      (x2,y2,w2,h2) <- object_geometry_get r1
      return (x1 == x2 && y1 == y2 && w1 == w2 && h1 == h2)


check_object_color canvas = do
   rect1 <- evas_object_rectangle_add canvas

   assertM "Coloring object" $ do
      let (r0,g0,b0,a0) = (10,20,30,40)
      object_color_set rect1 r0 g0 b0 a0
      (r1,g1,b1,a1) <- object_color_get rect1
      return (r0 == r1 && g0 == g1 && b0 == b1 && a0 == a1)


check_object_evas canvas = do
   r1 <- evas_object_rectangle_add canvas

   assertM "Retrieve object canvas" $ do
      (== canvas) <$> object_evas_get r1
