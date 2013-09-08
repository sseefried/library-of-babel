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

zoomToText :: IORef State -> String -> IO ()
zoomToText stateRef text =
  modifyIORef stateRef $ \s -> s { anim = Following (textToBabelChoices text)}

kbd :: [(Key, IORef State -> IO ())]
kbd = [(Char '\27', \_ -> exitWith ExitSuccess)
      ,(Char 'z', \stateRef -> do
                     zoomToText stateRef "THIS TEXT JUST WRITES ITSELF! AWESOME!"
                     postRedisplay Nothing)  ]  ++ moveHandlers


moveHandlers = map moveHandlerForDir [0..numSides - 1]
  where
    moveHandlerForDir n = 
      (Char (chr (ord '0' + n))
      , \stateRef -> do {
            s <- readIORef stateRef
          ; (case anim s of
              Still -> do
                writeIORef stateRef $ s { anim = Transitioning (Dir n) 0 stepsInAnimation Still }
                postRedisplay Nothing
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
data State = State { anim :: AnimState
                   , revChoices :: [Int]
                   , mdaSpec :: MultiDrawArraysSpec } 

data Dir = Dir Int -- Direction n
data AnimState = Still
               | Transitioning Dir Int Int AnimState {- previous nav -}
               | Following [Int] -- choices

data MultiDrawArraysSpec = MultiDrawArraysSpec { mdaIndices :: StorableArray Int ArrayIndex
                                               , mdaLengths :: StorableArray Int NumArrayIndices }




main :: IO ()
main = do
  getArgsAndInitialize
  w <- createWindow "Babel 2D"
  clientState VertexArray $= Enabled -- very important
  mdaSpec <- vertexArraySpec babelVertices
  stateRef <- newIORef $ State { anim = Still, revChoices = [], mdaSpec = mdaSpec }
  clearColor $= Color4 0 0 0 0
  currentColor $= Color4 1 1 1 1
  initialDisplayMode $= [DoubleBuffered]
  displayCallback $= display w stateRef
  keyboardMouseCallback $= Just (keyboard stateRef)
  windowSize $= Size 800 800
  mainLoop

(babelOrigins, babelVertices) = babelWorld 0.5

transitionStep :: IORef State -> IO ()
transitionStep stateRef = do
  s <- readIORef stateRef
  case anim s of 
    Transitioning _ _ _ _-> postRedisplay Nothing
    Following _          -> postRedisplay Nothing
    Still                -> return ()
  modifyIORef stateRef $ 
    \s -> case anim s of 
            Still -> s
            Transitioning d@(Dir i) n m prev ->
              if n < m then  s { anim = Transitioning d (n+1) m prev}
                       else  s { anim = prev, revChoices = i:revChoices s }
            Following []               -> s { anim = Still } 
            Following (choice:choices) ->  
              s { anim = Transitioning (Dir choice) 0 stepsInAnimation (Following choices) }



colorWorkaround :: IO ()
colorWorkaround = do
  currentColor $= Color4 0 1 0 (1 ::GLfloat)
  renderPrimitive Polygon . (mapM_ vertex) $ ([] :: [Vertex2 GLfloat])


interpolate :: Floating a => Int -> Int -> Vertex2 a -> ((a, a), a)
interpolate n m (Vertex2 fx fy) = ((-fx*frac, -fy*frac), 1 + 2*frac)
  where frac = fromIntegral n/fromIntegral m


drawPolygons :: State -> IO ()
drawPolygons s = do
  let m = mdaSpec s
  (_,i) <- getBounds (mdaLengths m)
  withStorableArray (mdaIndices m) $ \indicesPtr -> withStorableArray (mdaLengths m) $ \lensPtr -> do
    multiDrawArrays Polygon indicesPtr lensPtr (fromIntegral $ i+1)

display :: Window -> IORef State -> IO ()
display w stateRef = do
  s <- readIORef stateRef
  case anim s of 
    Still -> do
      loadIdentity
    Transitioning (Dir d) n m _ -> do
      loadIdentity
      let ((x,y), s) = interpolate n m (babelOrigins !! d)
      scale s s (1  :: GLfloat)
      translate (Vector3 x y (0.0 :: GLfloat))
    _ -> return ()
  -- now draw!
  clear [ColorBuffer, DepthBuffer]
  color (Color4 1 1 1 (1 :: GLfloat))
  drawPolygons s
  colorWorkaround
  rasterPos (Vertex2 (-0.2) (0.5 :: GLfloat))
  renderString Helvetica10 (babelToText . listToInteger .reverse $ revChoices s)
  swapBuffers
  transitionStep stateRef


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
vertexArraySpec :: [[Vertex2 GLfloat]] -> IO MultiDrawArraysSpec
vertexArraySpec vss = do
  let (fs, indices, lens) = aux 0 vss ([],[],[])
  a <- newListArray (0,length fs-1) fs
  withStorableArray a $ \ptr -> do
    arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 0 ptr
  indicesArray <- newListArray (0,length indices-1) (map fromIntegral indices)
  lensArray   <- newListArray (0,length lens-1)    (map fromIntegral lens)
  return $ MultiDrawArraysSpec { mdaIndices = indicesArray, mdaLengths =  lensArray }
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