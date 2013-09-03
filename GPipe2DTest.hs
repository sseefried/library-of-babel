module Main where

import Data.IORef
import GPipe2D
import Graphics.UI.GLUT hiding (RGB, RGBA, Shader)
import System.Exit


data State = State { radius :: Float, flag :: Bool } deriving Show

--circle ::  -> Image
circle (x,y) r pos = ifB (x'*x' + y'*y' <* r*r) white empty
  where x' = getX pos - x
        y' = getY pos - y
cOver :: FragColor -> FragColor -> FragColor
cOver (RGBA (r:.g:.b:.()) a) (RGBA (r':.g':.b':.()) a') =
  RGBA ((f r r'):.(f g g'):.(f b b'):.()) (f a a')
  where
    f :: Shader F Float -> Shader F Float -> Shader F Float
    f c c' = c*a + c'*(1.0 - a)
  
emptyI _ = empty

over :: Image -> Image -> Image
over im im' pos = im pos `cOver` im' pos

-- secondCircle s pos = ifB ( getX pos*getX pos + getY pos*getY pos <* (fromRational . toRational) (radius s)) (RGB $ 1:.0:.0:.()) black

-- babel :: Int -> Image
babel n r = aux n (0,0) r
  where
    aux 0 _     _ = emptyI
    aux n (x,y) r = foldl over emptyI [circle (x,y) r,
                                       aux (n-1) (x - d,y) (r/2),
                                       aux (n-1) (x + d,y) (r/2),
                                       aux (n-1) (x,y - d) (r/2),
                                       aux (n-1) (x,y + d) (r/2)
                                       ]
      where d = r*3

conv = fromRational . toRational

imageIO :: IORef State -> IO Image
imageIO stateRef = do
  s <- readIORef stateRef
  return $ babel 4 (conv $radius s)
--  return $ (if flag s then circle else secondCircle) s

main :: IO ()
main = do
  stateRef <- newIORef $ State { radius = 0.7, flag = True }
  gpipe2D "Test" (100:.100:.()) 500 stateRef imageIO initWindow

keyboard :: IORef State -> KeyboardMouseCallback
keyboard _ (Char '\27' ) Down _ _ = exitWith ExitSuccess
keyboard stateRef (Char 's') Down _ _ = modifyIORef stateRef $ \s -> s { flag = not (flag s)}
keyboard stateRef _ Down _ _         = do
  modifyIORef stateRef $ \s -> s { radius = radius s * 0.9 }
keyboard stateRef _ _ _ _ = return ()

initWindow :: IORef State -> Window -> IO ()
initWindow stateRef win = do
  keyboardMouseCallback $= Just (keyboard stateRef)
  idleCallback          $= Just (postRedisplay (Just win))
