module Main where

import System.Exit (exitFailure, exitSuccess)

import Control.Monad
import Control.Applicative
import Foreign.Ptr

import Graphics.Efl.Ecore
import Graphics.Efl.EcoreEvas
import Graphics.Efl.Evas
import Graphics.Efl.Eina

main = do
   check_object_clipping
   exitSuccess

assert :: String -> Bool -> IO ()
assert s b =
   if b then
      putStrLn $ s ++ ": PASS"
   else do
      putStrLn $ s ++ ": FAILED"
      exitFailure

assertM :: String -> IO Bool -> IO ()
assertM s b = assert s =<< b

check_object_clipping = do
   ecore_evas_init
   ee <- ecore_evas_new nullPtr 0 0 800 600 nullPtr
   canvas <- ecore_evas_get ee

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
