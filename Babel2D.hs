module Main where

import Graphics.Rendering.OpenGL hiding (rotate)
import Graphics.UI.GLUT hiding (rotate)
import System.Exit


keyboard :: KeyboardMouseCallback
keyboard (Char '\27' ) Down _ _ = exitWith ExitSuccess
keyboard (Char 'a') Down _ _ = do

  ortho2D (-0.9) (0.5) (-0.9) 0.5
keyboard  _ _ _ _ = return ()

main :: IO ()
main = do
  getArgsAndInitialize
  w <- createWindow "Babel 2D"
  displayCallback $= display
  idleCallback $= Just flush
  keyboardMouseCallback $= Just keyboard
  windowSize $= Size 800 800
  idleCallback $= Just (postRedisplay (Just w))


--  ortho2D (-1) 1 (-1) 1
  mainLoop

display :: IO ()
display = do
  clearColor $= Color4 0 0 0 0
  currentColor $= Color4 1 0 1 1
  clear [ColorBuffer]
  mapM_ (renderPrimitive Polygon . (mapM_ vertex)) (babel 8 0.5)
  flush

-- rotate a 2D point about the origin by t
rotate :: Floating a => a -> Vertex2 a -> Vertex2 a
rotate t (Vertex2 x y) = Vertex2 (cos t*x - sin t*y) (sin t*x + cos t*y)

about :: Num a => (Vertex2 a -> Vertex2 a) -> Vertex2 a -> (Vertex2 a -> Vertex2 a)
(f `about` Vertex2 x y) (Vertex2 x' y') =
                             case f (Vertex2 (x'-x) (y' - y)) of
                                (Vertex2 x'' y'') -> Vertex2 (x'' + x) (y'' + y)

rotateAbout :: Floating a => a -> Vertex2 a -> Vertex2 a -> Vertex2 a
rotateAbout t v v' = (rotate t `about` v) v'

--
-- A list of polygons definining the Library of Babel.
--
babel :: Int -> GLfloat -> [[Vertex2 GLfloat]]
babel n r = aux n r (Vertex2 0 0)
  where
    aux 0 _ _ = []
    aux n r o@(Vertex2 x y) = hexagon o r:concatMap (aux (n-1) (r/3)) origins
      where
        origins = map (\ang -> rotateAbout (ang+pi/6) o (Vertex2 (x+2.1*r) y)) hexAngles

hexAngles :: [GLfloat]
hexAngles = [0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3]

hexagon :: Vertex2 GLfloat -> GLfloat -> [Vertex2 GLfloat]
hexagon o@(Vertex2 x y) r = map (\ang -> rotateAbout ang o (Vertex2 (r+x) y)) hexAngles

myPoints :: [(GLfloat, GLfloat)]
myPoints = [ (-0.25, 0.25) 
           , (0.75, 0.35)
           , (0.75, -0.15)
           , ((-0.75), -0.25)]