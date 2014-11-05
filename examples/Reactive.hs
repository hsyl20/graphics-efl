import Graphics.Efl.Simple
import Control.Monad (void,when)
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Traversable (traverse)

import Graphics.Efl.Core.Idle

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.STM.LinkedList as LL

data PropAction a = PropSet a | PropKill

data Property a = Property
   { propSource :: STM (PropAction a)
   , propSetter :: a -> STM ()
   , propValue :: TVar a
   }

initProperty :: Eq a => Property a -> IO ()
initProperty p = void $ forkIO $ g
   where 
      g = do
         b <- f
         if b then g else return ()

      f = atomically $ do
         -- get the value
         v <- propSource p
         -- compare to the old one
         old <- readTVar (propValue p)
         case (old,v) of
            -- this property is dead, do nothing
            (_, PropKill) -> return False

            -- a different value has been set
            (x, PropSet y) | x /= y -> do
               writeTVar (propValue p) y
               propSetter p y
               return True

            -- the value is the same as the old one
            (x, PropSet y) | otherwise -> retry

readProperty :: Property a -> STM a
readProperty = readTVar . propValue

   
createProperty :: Eq a => STM (PropAction a) -> (a -> STM ()) -> IO (Property a)
createProperty src setter = do
   init' <- atomically $ src
   init'' <- case init' of
      PropKill -> error "Cannot obtain property initial value (instantaneously killed)"
      PropSet x  -> return x
   p <- atomically $ do
      setter init''
      Property src setter <$> newTVar init''
   initProperty p
   return p
   

main :: IO ()
main = do
   initWindowingSystem
   engines <- getEngines
   putStrLn (show engines)

   let engine = if "opengl_x11" `elem` engines 
         then Just "opengl_x11"
         else Nothing

   withDefaultWindow engine $ \ win canvas -> do
      engineName <- getEngineName win
      putStrLn $ "Using engine " ++ engineName

      setWindowTitle "Simple Haskell-EFL Example" win

      ll <- atomically $ LL.empty

      let deleteAll xs = do
            e <- LL.start xs
            case e of
               Nothing -> return ()
               Just x  -> LL.delete x >> deleteAll xs

      _ <- addIdleEnterer $ do
         elems <- atomically $ do
            es <- LL.toList ll
            deleteAll ll
            return es

         traverse_ id elems
         return True

      bg <- addRectangle canvas
            |> setObjectColor (0,0,0,255)
            |> setLayer (-1)
            |> uncover
            |> setFocus True
            |> onKeyDown (\ _ ev -> do
                  name <- keyDownKeyName ev
                  case name of
                     "Escape" -> quitMainLoop
                     _ -> return ()
                  )

      onWindowResize win $ do
         (_,_,w,h) <- getWindowGeometry win
         resize w h bg


      r <- addRectangle canvas
            |> setName "First rectangle"
            |> resize 100 30
            |> setLayer 2
            |> setObjectColor (255,0,0,255)
            |> uncover
      
      r2 <- addRectangle canvas
            |> setName "Second rectangle"
            |> resize 100 30
            |> setLayer 2
            |> setObjectColor (0,255,0,255)
            |> uncover

      selr <- addRectangle canvas
            |> setName "Second rectangle"
            |> resize 120 40
            |> move 300 40
            |> setObjectColor (255,255,255,255)
            |> enableEventPassing

      selectedPos <- newTVarIO Nothing

      let act f = void $ LL.append f ll

      -- r position
      p1 <- createProperty
         (return (PropSet (20,40)))
         (\(x,y) -> act $ move x y r)
      
      -- r2 position
      p2 <- createProperty
         (return (PropSet (300,40)))
         (\(x,y) -> act $ move x y r2)

      -- selr position
      void $ createProperty 
         (do
            s <- traverse readProperty =<< readTVar selectedPos
            case s of
               -- Nothing selected
               Nothing -> return (PropSet (0,0))
               Just (x,y)  -> return (PropSet (x-10,y-5))
         )

         (\(x,y) -> act $ move x y selr)
      
      -- selr uncover
      void $ createProperty 
         (do
            s <- readTVar selectedPos
            case s of
               Nothing -> return (PropSet False)
               Just b  -> return (PropSet True)
         )

         (\visible -> when (visible) (act $ uncover selr))

      let clickHandler p obj ev = do
            name <- getName obj
            (x,y) <- getObjectPosition obj
            btn <- mouseDownButton ev
            let str = "You clicked on " ++ name ++ " with button " ++ show btn
            let actions = 
                  [ print str
                  , atomically $ writeTVar selectedPos (Just p)
                  , uncover selr
                  ]

            _ <- atomically $ traverse_ (`LL.append` ll) actions

            return ()

      onMouseDown (clickHandler p1) r
      onMouseDown (clickHandler p2) r2

      return ()
