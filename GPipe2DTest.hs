module Main where

import Data.IORef
import GPipe2D
import Graphics.UI.GLUT
import System.Exit


data State = State { radius :: Float } deriving Show

circle :: State -> Image
circle s pos = ifB ( getX pos*getX pos + getY pos*getY pos <* (fromRational . toRational) (radius s)) white black


main :: IO ()
main = do
  stateRef <- newIORef $ State { radius = 0.7 }
  gpipe2D "Test" (100:.100:.()) 500 stateRef circle initWindow

keyboard :: IORef State -> KeyboardMouseCallback
keyboard _ (Char '\27' ) Down _ _ = exitWith ExitSuccess
keyboard stateRef _ _ _ _         = do
  modifyIORef stateRef $ \s -> State { radius = radius s * 0.9}

initWindow :: IORef State -> Window -> IO ()
initWindow stateRef win = do
  keyboardMouseCallback $= Just (keyboard stateRef)
  idleCallback          $= Just (postRedisplay (Just win))
