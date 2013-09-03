module GPipe2D (
  Image,
  getX, getY,
  module Graphics.GPipe,
  module Graphics.UI.GLUT,
  module Vec,
  gpipe2D,
  white, black
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

type Image = Vec3 (Fragment Float) -> Color RGBFormat (Fragment Float)

triangle :: PrimitiveStream Triangle (Vec3 (Vertex Float))
triangle = toGPUStream TriangleStrip $
          [   1.0  :.   1.0  :. 0.0 :.(),
            (-1.0) :.   1.0  :. 0.0 :.(), 
              1.0  :. (-1.0) :. 0.0 :.(),
            (-1.0) :. (-1.0) :. 0.0 :.() ]


getX, getY :: Vec3 (Fragment Float) -> Fragment Float
getX = Vec.get n0
getY = Vec.get n1


white, black :: Color RGBFormat (Fragment Float)
white = RGB $ 1:.1:.1:.()
black = RGB $ 0:.0:.0:.()



-- This implements the vertex shader
procTriangle :: Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), Vec3 (Vertex Float))
procTriangle size = fmap (projTriangle size) triangle 

projTriangle :: Vec2 Int -> Vec3 (Vertex Float) -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
projTriangle size pos = (orthoProj `multmv` homPos, pos)
    where homPos = homPoint pos :: Vec4 (Vertex Float)  
          orthoProj = toGPU $ orthogonal (-10) 10  (2:.2:.())

-- This implements the fragment shader
rastTriangle :: Image -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
rastTriangle image size = fmap (\(front,pos) -> image pos) $ rasterizeFrontAndBack $ procTriangle size 

triangleFrame :: Image -> Vec2 Int -> FrameBuffer RGBFormat () ()
triangleFrame image size = draw (rastTriangle image size) clear
    where
      draw  = paintColor NoBlending (RGB $ Vec.vec True)  
      clear = newFrameBufferColor (RGB (0.0:.0.0:.0.0:.()))

--
-- gpipe2D
--
gpipe2D :: String -> Vec2 Int -> Int -> IORef s -> (s -> Image)
        -> (IORef s -> Window -> IO ()) -> IO ()
gpipe2D windowName pos width stateRef imageFun initWindow = do
  getArgsAndInitialize
  newWindow windowName 
       pos
       (width:.width:.()) 
       (renderFrame stateRef imageFun)
       (initWindow stateRef)
  mainLoop

{- displayCallbackForImage :: Image -> Window -> IO ()
displayCallbackForImage image w = do
  cache <- liftM fromJust $ getContextCache w --We need to do this to get the correct size
  let Size x y = contextViewPort cache
  FrameBuffer io <- renderFrame image (fromIntegral x :. fromIntegral y :. ())
  runReaderT io cache
  GLUT.swapBuffers -}

renderFrame :: IORef s -> (s -> Image) -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame stateRef imageFun size = do
          s <- readIORef stateRef
          return $ triangleFrame (imageFun s) size

