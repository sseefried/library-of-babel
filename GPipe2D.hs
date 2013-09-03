module GPipe2D (
  Image, FragColor,
  getX, getY,
  module Graphics.GPipe,
  module Graphics.UI.GLUT,
  module Vec,
  gpipe2D,
  white, black, empty
) where

import Graphics.GPipe
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Vec.LinAlg
import Data.Monoid
import Data.IORef
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Graphics.UI.GLUT( Window,
                         mainLoop,
                         postRedisplay,
                         idleCallback,
                         getArgsAndInitialize,
                         ($=))


type FragColor = Color RGBAFormat (Fragment Float)
type Image = Vec3 (Fragment Float) -> FragColor

triangle :: PrimitiveStream Triangle (Vec3 (Vertex Float))
triangle = toGPUStream TriangleStrip $
          [   1.0  :.   1.0  :. 0.0 :.(),
            (-1.0) :.   1.0  :. 0.0 :.(), 
              1.0  :. (-1.0) :. 0.0 :.(),
            (-1.0) :. (-1.0) :. 0.0 :.() ]


getX, getY :: Vec3 (Fragment Float) -> Fragment Float
getX = Vec.get n0
getY = Vec.get n1


white, black, empty :: FragColor
white = RGBA (1:.1:.1:.()) 1
black = RGBA (0:.0:.0:.()) 1
empty = RGBA (0:.0:.0:.()) 0



-- This implements the vertex shader
procTriangle :: Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), Vec3 (Vertex Float))
procTriangle size = fmap (projTriangle size) triangle 

projTriangle :: Vec2 Int -> Vec3 (Vertex Float) -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
projTriangle size pos = (orthoProj `multmv` homPos, pos)
    where homPos = homPoint pos :: Vec4 (Vertex Float)  
          orthoProj = toGPU $ orthogonal (-10) 10  (2:.2:.())

-- This implements the fragment shader
rastTriangle :: Image -> Vec2 Int -> FragmentStream FragColor
rastTriangle image size = fmap (\(front,pos) -> image pos) $ rasterizeFrontAndBack $ procTriangle size 

triangleFrame :: Image -> Vec2 Int -> FrameBuffer RGBAFormat () ()
triangleFrame image size = draw (rastTriangle image size) clear
    where
      draw  = paintColor NoBlending (RGBA (Vec.vec True) True)
      clear = newFrameBufferColor (RGBA (0.0:.0.0:.0.0:.()) 0)

--
-- gpipe2D
--
gpipe2D :: String -> Vec2 Int -> Int -> IORef s -> (IORef s -> IO Image)
        -> (IORef s -> Window -> IO ()) -> IO ()
gpipe2D windowName pos width stateRef imageIO initWindow = do
  getArgsAndInitialize
  newWindow
    windowName 
    pos
    (width:.width:.()) 
    (renderFrame stateRef imageIO)
    (initWindow stateRef)
  mainLoop

{- displayCallbackForImage :: Image -> Window -> IO ()
displayCallbackForImage image w = do
  cache <- liftM fromJust $ getContextCache w --We need to do this to get the correct size
  let Size x y = contextViewPort cache
  FrameBuffer io <- renderFrame image (fromIntegral x :. fromIntegral y :. ())
  runReaderT io cache
  GLUT.swapBuffers -}

renderFrame :: IORef s -> (IORef s -> IO Image) -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
renderFrame stateRef imageIO size = do
          image <- imageIO stateRef
          return $ triangleFrame image size

