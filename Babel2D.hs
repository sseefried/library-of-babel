module Main where

--import Graphics.Rendering.OpenGL hiding (rotate, Position)
import Graphics.UI.GLUT hiding (rotate, Position)
import System.Exit
import Data.IORef
import Data.Char
import Data.Array.Storable
import Foreign.Storable
import Foreign.C.Types (CInt)
import Graphics.UI.GLUT.Fonts
import Babel



numSides :: Int
numSides = 6

stepsInAnimation :: Int
stepsInAnimation = 25


kbd :: [(Key, IORef State -> IO ())]
kbd = [(Char '\27', \_ -> exitWith ExitSuccess)] ++ moveHandlers

moveHandlers = map moveHandlerForDir [0..numSides - 1]
  where
    moveHandlerForDir n = 
      (Char (chr (ord '0' + n))
      , \stateRef -> do {
            s <- readIORef stateRef
          ; (case pos s of
              Still -> writeIORef stateRef $ s { pos = Transitioning (Dir n) 0 stepsInAnimation }
              _     -> return ())
        })

keyboard :: IORef State -> KeyboardMouseCallback
keyboard stateRef c Down _ _ = do
   case lookup c kbd of
     Just handler -> handler stateRef
     Nothing -> return ()
keyboard _ _ _ _ _ = return ()     

--
-- We have a simple state machine.
-- Either, you are stationary on a hexagon or you are 
-- transitioning between it, at step n of m.
--
data State = State { pos :: Position, choices :: [Int]
                   , indices :: StorableArray Int ArrayIndex
                   , lens :: StorableArray Int NumArrayIndices} 
data Dir = Dir Int -- Direction n
data Position = Still
              | Transitioning Dir Int Int

main :: IO ()
main = do
  getArgsAndInitialize
  w <- createWindow "Babel 2D"
  clientState VertexArray $= Enabled -- very important
  (indices, lens) <- vertexArraySpec babelVertices
  stateRef <- newIORef $ State { pos = Still, choices = [], indices = indices, lens = lens }
  clearColor $= Color4 0 0 0 0
  currentColor $= Color4 1 1 1 1
  initialDisplayMode $= [DoubleBuffered]

--  materialAmbient Front   $= Color4 0.1745 0.01175 0.001175 1
--  materialDiffuse Front   $= Color4 0.61424 0.04136 0.04136 1
--  materialSpecular Front  $= Color4 0.727811 0.626959 0.626950 1
--  materialShininess Front $= 0.6
--  position (Light 0)      $= Vertex4 0 0 1000 1

--  lineSmooth $= Enabled
--  blend $= Enabled

--  ambient (Light 0) $= Color4 0.3 0.3 0.3 1
--  specular (Light 0) $= Color4 1.0 1.0 1.0 1
--  diffuse (Light 0) $= Color4 1.0 1.0 1.0 1

--  normalize $= Enabled
----  lighting $= Enabled
--  light (Light 0) $= Enabled
--  depthFunc $= Just Less

  displayCallback $= display w stateRef
  keyboardMouseCallback $= Just (keyboard stateRef)
  windowSize $= Size 800 800
  mainLoop

(babelOrigins, babelVertices) = babelWorld 0.5

transitionStep :: IORef State -> IO ()
transitionStep stateRef = do
  modifyIORef stateRef $ 
    \s -> case pos s of 
            Still -> s
            Transitioning d@(Dir i) n m -> if n < m then  s { pos = Transitioning d (n+1) m }
                                            else  s { pos = Still, choices = i:choices s }

colorWorkaround :: IO ()
colorWorkaround = do
  currentColor $= Color4 0 1 0 (1 ::GLfloat)
  renderPrimitive Polygon . (mapM_ vertex) $ ([] :: [Vertex2 GLfloat])


interpolate :: Floating a => Int -> Int -> Vertex2 a -> ((a, a), a)
interpolate n m (Vertex2 fx fy) = ((-fx*frac, -fy*frac), 1 + 2*frac)
  where frac = fromIntegral n/fromIntegral m


drawPolygons :: State -> IO ()
drawPolygons s = do
  (_,i) <- getBounds (lens s)
  withStorableArray (indices s) $ \indicesPtr -> withStorableArray (lens s) $ \lensPtr -> do
    multiDrawArrays Polygon indicesPtr lensPtr (fromIntegral $ i+1)

display :: Window -> IORef State -> IO ()
display w stateRef = do
  s <- readIORef stateRef
  clear [ColorBuffer, DepthBuffer]
  color (Color4 1 1 1 (1 :: GLfloat))
  drawPolygons s
  
--  mapM_ (renderPrimitive Polygon . (mapM_ vertex)) babelVertices
  case pos s of 
    Still -> loadIdentity
    Transitioning (Dir d) n m -> do
      loadIdentity
      let ((x,y), s) = interpolate n m (babelOrigins !! d)
      scale s s (1  :: GLfloat)
      translate (Vector3 x y (0.0 :: GLfloat))
  transitionStep stateRef
  colorWorkaround
  rasterPos (Vertex2 (-0.2) (0.5 :: GLfloat))
  renderString Helvetica10 (babelToText . listToInteger .reverse $ choices s)
  swapBuffers
  postRedisplay (Just w)

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
--
--
vertexArraySpec :: [[Vertex2 GLfloat]] -> IO (StorableArray Int ArrayIndex,
                                                StorableArray Int NumArrayIndices)
vertexArraySpec vss = do
  let (fs, indices, lens) = aux 0 vss ([],[],[])
  a <- newListArray (0,length fs-1) fs
  withStorableArray a $ \ptr -> do
    arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 0 ptr
  indicesArray <- newListArray (0,length indices-1) (map fromIntegral indices)
  lensArray   <- newListArray (0,length lens-1)    (map fromIntegral lens)
  return (indicesArray, lensArray)
  where

aux :: Int -> [[Vertex2 GLfloat]]  ->   ([[GLfloat]], [Int], [Int]) -> ([GLfloat], [Int], [Int])
aux _ []       (revFss,revIndices,revLens) = ((concat . reverse $ revFss), reverse revIndices, reverse revLens)
aux n (vs:vss) (revFss,revIndices,revLens) = aux (n+len) vss (newFs:revFss,n:revIndices,len:revLens)
  where
    len = length vs
    newFs :: [GLfloat]
    newFs = concatMap (\(Vertex2 x y) -> [x,y]) vs
--
-- A list of polygons definining the Library of Babel.
--
babelWorld :: GLfloat -> ([Vertex2 GLfloat],[[Vertex2 GLfloat]])
babelWorld r = (originsForNgonAngles r center, aux r center)
  where
    center = Vertex2 0 0 
    originsForNgonAngles :: GLfloat -> Vertex2 GLfloat -> [Vertex2 GLfloat]
    originsForNgonAngles r o@(Vertex2 x y) = 
      map (\ang -> rotateAbout (ang+pi/(fromIntegral numSides)) o (Vertex2 (x+2*r) y)) (ngonAngles numSides)
    aux r o@(Vertex2 x y) 
      | r < 0.001= []

      | otherwise = ngon numSides o r:concatMap (aux (2*r/(fromIntegral numSides))) origins
      where
        origins = originsForNgonAngles r o

ngonAngles :: Floating a => Int -> [a]
ngonAngles n = map (((2*pi/fromIntegral n)*) . fromIntegral) [0..n-1]

ngon :: Int -> Vertex2 GLfloat -> GLfloat -> [Vertex2 GLfloat]
ngon n o@(Vertex2 x y) r = map (\ang -> rotateAbout ang o (Vertex2 (r+x) y)) (ngonAngles n)