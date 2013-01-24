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

   assertM "Layer object get . set == id" $ do
      object_layer_set r1 10
      (== 10) <$> object_layer_get r1


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
