module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Foreign.Storable
import Data.Array.Storable
import Data.Array.MArray

main :: IO ()
main = do
  getArgsAndInitialize
  w <- createWindow "Vertex Array Test"
  clearColor $= Color4 0 0 0 0
  currentColor $= Color4 1 1 1 1
  windowSize $= Size 800 800
  clientState VertexArray $= Enabled
  foo
  displayCallback $= display w  
  mainLoop


mkVertices :: (GLfloat, GLfloat) -> [GLfloat]
mkVertices (x,y) = [ -0.25 + x, -0.25 +y , 0.25 + x, -0.25 + y,
             0.25+x, 0.25+y, -0.25+x, 0.25+y]

vertices = mkVertices (-0.5, 0.5) ++ mkVertices (0.5, -0.5)


foo :: IO ()
foo = do
  a <- newListArray (0,length vertices-1) vertices
  withStorableArray a $ \ptr -> do
    arrayPointer VertexArray $= VertexArrayDescriptor 
                                  2
                                  Float
                                  0 -- 0 = stride automatically computed by OpenGL
--                                  (fromIntegral $ 2*sizeOf (undefined ::GLfloat)) -- stride is numComponents * vertices in polygon
                                  ptr


display :: Window -> IO ()
display w = do
  clear [ColorBuffer, DepthBuffer]
  drawArrays Polygon 0 4
  drawArrays Polygon 4 4 
  flush

  postRedisplay (Just w)
  return ()