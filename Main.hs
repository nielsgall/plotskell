
-- compile note:
--   ghc -O2 -fllvm -fforce-recomp "Main.hs"

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Applicative
import           Control.Monad.RWS.Strict     (liftIO)
import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           Graphics.GLUtil
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)


import           Foreign.Ptr
import           Control.Concurrent (threadDelay)
import           GHC.Float
import qualified Data.Vector.Storable as SV
import qualified Graphics.Rendering.OpenGL.Raw.ARB.ShaderObjects as SO
import           Foreign.Storable (sizeOf)


import           Data.Word (Word8, Word16)

import           System.Environment (getArgs)

import           GhettoLM
import           MathParser
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as M
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Global variables

data Shaders = Shaders { vertexShader              :: !Shader
                       , fragmentShader            :: !Shader
                       , getProgram                :: !Program
                       , attribute_coord2d         :: !AttribLocation
                       , uniform_mytexture         :: !UniformLocation
                       , uniform_vertex_transform  :: !UniformLocation
                       , uniform_texture_transform :: !UniformLocation
                       , uniform_color             :: !UniformLocation
                       }

data DOFShader = DOFShader { renderedTexture    :: !UniformLocation
                           , depthTexture       :: !UniformLocation
                           }

data Resources = Resources { vbo_vertices :: !BufferObject
                           , vbo_indices  :: !BufferObject
                           , vbo_polyind  :: !BufferObject
                           , shaders      :: !Shaders
                           , mytexture    :: !TextureObject
                           , offsetX      :: !GLfloat
                           , offsetY      :: !GLfloat
                           , scaleX       :: !GLfloat
                           , interpolate  :: !Bool
                           , drawpoints   :: !Bool
                           , clamping     :: !Bool
                           }

data State = State { dt         :: !GLfloat
                   , graphIndex :: !Int
                   }

data Bound = Bound Char P.Number P.Number --Integer Integer
    deriving Show


gl_n = 256 :: Float 
lsize = 256 :: Float

windowWidth = 640
windowHeight = 480

gsize = 100

-- End of global variables
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Utility functions

convertFloat :: Float -> GLfloat
convertFloat = realToFrac

convertIntToCint :: Int -> GLint
convertIntToCint = fromIntegral

mapRange :: (Fractional a) => (a, a) -> (a, a) -> a -> a
mapRange (a1, a2) (b1, b2) s = b1 + (s - a1) * (b2 - b1) / (a2 - a1)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- End of utility functions
-------------------------------------------------------------------------------

grapho :: [Float]
grapho = [ (z * 127.0 + 128.0) | i <- [0..(gl_n-1)],
                                      j <- [0..(gl_n-1)],
                                        let x = ((i - gl_n / 2) / (gl_n / 2.0)),
                                        let y = ((j - gl_n / 2) / (gl_n / 2.0)),
                                        let d = sqrt (x^2 + y^2) * 4.0,
                                        let z = (1 - d * d) * exp (d * d / (-2.0))]





--tList1 :: [???]
tList1 = [ x | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let a = mapRange (0, (gl_n - 1)) (0, (2 * pi)) j,
                 let b = mapRange (0, (gl_n - 1)) (0, (2 * pi)) i,
                 let e = cos a,
                 let f = cos b,
                 let g = sin a,
                 let x = (e * f) + g ]

tList2 = [ z | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let a = mapRange (0, (gl_n - 1)) (-1, 1) j,
                 let b = mapRange (0, (gl_n - 1)) (-1, 1) i,
                 let x = a^^2,
                 let y = b^^2,
                 let z = x + y ]

tList3 = [ z | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let a = mapRange (0, (gl_n - 1)) (-3, 3) j,
                 let b = mapRange (0, (gl_n - 1)) (-3, 3) i,
                 let x = a^^2,
                 let y = b^^2,
                 let z = (sin (x + y)) ]-- / 10 ]

tList4 = [ z | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let a = mapRange (0, (gl_n - 1)) (-14, 14) j,
                 let b = mapRange (0, (gl_n - 1)) (-14, 14) i,
                 let x = a^^2,
                 let y = b^^2,
                 let z = (sin (sqrt (x + y))) / (sqrt (x + y)) ]-- / 10 ]

tList5 = [ z | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let x = mapRange (0, (gl_n - 1)) (-3, 3) j,
                 let y = mapRange (0, (gl_n - 1)) (-2, 2) i,
                 let z = (sin (x + (y^^2))) ]

tList6 = [ z | i <- [0..(gl_n - 1)],
               j <- [0..(gl_n - 1)],
                 let z = 1 ]



--toGraph :: [] -> [Word8]
toGraph l = (map (round . mapRange (min, max) (0, (gl_n - 1))) $ l)
    where
        min = minimum l
        max = maximum l

pointslist :: [[Word8]]
pointslist = map toGraph [tList1, tList2, tList3, tList4, tList5, grapho]



-- Vertex methods
verticesX :: [GLfloat]
verticesX  = [ (j - (gsize / 2)) / (gsize / 2.0) | i <- [0..gsize], j <- [0..gsize] ]

verticesY :: [GLfloat]
verticesY  = [ (i - (gsize / 2)) / (gsize / 2.0) | i <- [0..gsize], j <- [0..gsize] ]

vertices :: [GLfloat]
vertices  = merge verticesX verticesY


-- Indice methods
indicesX  :: [Word16]
indicesX   = [ y       * (gsize + 1) + x     | y <- [0..gsize], x <- [0..(gsize - 1)] ]

indicesY  :: [Word16]
indicesY   = [ y       * (gsize + 1) + x + 1 | y <- [0..gsize], x <- [0..(gsize - 1)] ]

indicesX2 :: [Word16]
indicesX2  = [ y       * (gsize + 1) + x     | x <- [0..gsize], y <- [0..(gsize - 1)] ]

indicesY2 :: [Word16]
indicesY2  = [ (y + 1) * (gsize + 1) + x     | x <- [0..gsize], y <- [0..(gsize - 1)] ]

indices :: [Word16]
indices  = indices1 ++ indices2
indices  = (merge indicesX indicesY) ++ (merge indicesX2 indicesY2)


polyindices :: [Word16]
polyindices = yseq [] [0..gsize] [0..(gsize - 1)]

vectorpolyindices :: SV.Vector Word16
vectorpolyindices = SV.fromList $! polyindices


xseq acc _   []   = acc
xseq acc y (x:xs) =
    let a = y       * (gsize + 1) + x
        b = y       * (gsize + 1) + x + 1
        c = (y + 1) * (gsize + 1) + x + 1
        d = (y + 1) * (gsize + 1) + x
        newacc = acc ++ [a] ++ [b] ++ [c] ++ [a] ++ [c] ++ [d]
    in newacc `seq` xseq newacc y xs

yseq result   []   _  = result
yseq result (y:ys) xl =
    let innerResult = result ++ (xseq [] y xl)
    in seq innerResult $ yseq innerResult ys xl


mat4ToVectorList :: Mat4 -> SV.Vector GLfloat
mat4ToVectorList (Mat4 (Vec4 a b c d)
                       (Vec4 e f g h)
                       (Vec4 i j k l)
                       (Vec4 m n o p)) = result
    where
        result = SV.fromList [ a, b, c, d
                             , e, f, g, h
                             , i, j, k, l
                             , m, n, o, p ] :: SV.Vector GLfloat


makeTexture :: [Word8] -> IO TextureObject
makeTexture points = 
    do putStrLn "maketexture start"
       let wh = (round gl_n) :: Int
       texture <- loadTexture $ texInfo wh wh TexMono points --graph
       textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
       putStrLn "maketexture end"
       return texture


initRenderShaders = do vs <- loadShader VertexShader   $ "shaders" </> "vs.glsl"
                       fs <- loadShader FragmentShader $ "shaders" </> "fs.glsl"
                       p <- linkShaderProgram [vs, fs]
                       Shaders vs fs p
                           <$> get (attribLocation p "coord2d")
                           <*> get (uniformLocation p "mytexture")
                           <*> get (uniformLocation p "vertex_transform")
                           <*> get (uniformLocation p "texture_transform")
                           <*> get (uniformLocation p "color")


makeResources pointList = Resources
                    <$> makeBuffer ArrayBuffer vertices
                    <*> makeBuffer ArrayBuffer indices
                    <*> fromVector ArrayBuffer vectorpolyindices
                    <*> initRenderShaders
                    <*> initDOFShader
                    <*> makeTexture pointList
                    <*> pure 0.0    -- offsetX
                    <*> pure 0.0    -- offsetY
                    <*> pure 1.0    -- scaleX
                    <*> pure False  -- interpolate
                    <*> pure False  -- drawpoints
                    <*> pure False  -- clamping

makeState = State
                <$> pure 0.0
                <*> pure 0


setupPoints :: Resources -> State -> IO ()
setupPoints r s = let 
                    -- textures
                    tex  = (mytexture r)
                    texu = uniform_mytexture (shaders r)

                    -- model
                    model = gluMakeIdentity

                    -- view
                    eye    = Vec3 0.0 (-2.0 :: GLfloat) 2.0
                    center = Vec3 0.0 0.0 0.0
                    up     = Vec3 0.0 0.0 1.0
                    view   = gluLookAt eye center up

                    -- projection
                    fovy       = 45.0 :: GLfloat
                    aspect     = (windowWidth / windowHeight) :: GLfloat
                    zNear      = 0.1 :: GLfloat
                    zFar       = 10.0 :: GLfloat
                    projection = gluPerspective fovy aspect zNear zFar

                    -- mvp
                    vertex_transform =
                        multMat4s (transpose projection) $ multMat4s (transpose view) model

                    scaledMat4 =
                        scaleMat4 gluMakeIdentity (Vec3 (scaleX r) (scaleX r) 1.0)
                    texture_transform =
                        translateMat4 scaledMat4 (Vec3 (offsetX r) (offsetY r) 0.0)

                    vt = mat4ToVectorList $ vertex_transform
                    tt = mat4ToVectorList $ texture_transform

                    coord2d  = attribute_coord2d (shaders r)
                    prog     = getProgram (shaders r)

                    interpolatingVar = (interpolate r)
                    clampingVar      = (clamping r)
                    drawpointsVar    = (drawpoints r)

                    stride   = 0
                    vad      = VertexArrayDescriptor 2 Float stride offset0
                in do currentProgram $= Just (getProgram (shaders r))
                      activeTexture $= TextureUnit 0
                      textureBinding Texture2D $= Just tex
                      uniform texu $= Index1 (0 :: GLint)
                        
                      if (interpolatingVar)
                          then
                              textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
                          else
                              textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)

                      if (clamping r)
                          then
                              textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
                          else
                              textureWrapMode Texture2D S $= (Repeated, Repeat)

                      if (clamping r)
                          then
                              textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
                          else
                              textureWrapMode Texture2D T $= (Repeated, Repeat) 

                      (UniformLocation v_trnsfrm) <- get (uniformLocation prog "vertex_transform")
                      SV.unsafeWith vt (\ptr -> SO.glUniformMatrix4fv v_trnsfrm 1 0 ptr)
                      (UniformLocation t_trnsfrm) <- get (uniformLocation prog "texture_transform")
                      SV.unsafeWith tt (\ptr -> SO.glUniformMatrix4fv t_trnsfrm 1 0 ptr)

                      polygonOffset $= (1.0, 1.0)
                      polygonOffsetFill $= Enabled

                      let
                        colorWhite = SV.fromList [0.5, 0.5, 0.5, 1.0] :: SV.Vector GLfloat -- white

                      (UniformLocation clr) <- get (uniformLocation prog "color")
                      SV.unsafeWith colorWhite (\white -> SO.glUniform4fv clr 1 white)

                      bindBuffer ArrayBuffer      $= Just (vbo_vertices r)
                      vertexAttribPointer coord2d $= (ToFloat, vad)
                      vertexAttribArray coord2d   $= Enabled


                      bindBuffer ElementArrayBuffer $= Just (vbo_polyind r)
                      if (drawpointsVar)
                          then
                              drawElements Triangles 0 UnsignedShort offset0
                          else
                              drawElements Triangles (gsize * gsize * 6) UnsignedShort offset0


                      let
                        clrBright = SV.fromList [2.0, 2.0, 2.0, 1.0] :: SV.Vector GLfloat -- white

                      (UniformLocation clr) <- get (uniformLocation prog "color")
                      SV.unsafeWith clrBright (\bright -> SO.glUniform4fv clr 1 bright)

                      bindBuffer ElementArrayBuffer $= Just (vbo_indices r)
                      if (drawpointsVar)
                          then
                              drawElements Points (gsize * (gsize + 1) * 4) UnsignedShort offset0
                          else
                              drawElements Lines  (gsize * (gsize + 1) * 4) UnsignedShort offset0


drawInit :: IORef Resources -> IO ()
drawInit r' = do blend $= Enabled
                 blendFunc $= (SrcAlpha, OneMinusSrcAlpha)


draw :: IORef Resources -> IORef State -> IO ()
draw r' s' = do r <- readIORef r'
                s <- readIORef s'
                setupPoints r s


-- Keyboard input handler
keyCallback :: IORef Resources -> IORef State -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback ioref state window key scancode action mods = when 
    (action == GLFW.KeyState'Pressed || action == GLFW.KeyState'Repeating) $ do
        when (key == GLFW.Key'Q) $ do
            GLFW.setWindowShouldClose window True
            putStrLn "q"
        when (key == GLFW.Key'A) $ do
            r <- readIORef ioref
            s <- readIORef state
            let currentOffset = (offsetX r)
            let ct            = (dt s)
            modifyIORef ioref (\x -> x { offsetX = (currentOffset - 0.03) :: GLfloat })
            --putStrLn "a"
        when (key == GLFW.Key'D) $ do
            r <- readIORef ioref
            s <- readIORef state
            let currentOffset = (offsetX r)
            let ct            = (dt s)
            modifyIORef ioref (\x -> x { offsetX = (currentOffset + 0.03) :: GLfloat })
            --putStrLn "d"
        when (key == GLFW.Key'W) $ do
            r <- readIORef ioref
            s <- readIORef state
            let currentOffset = (offsetY r)
            let ct            = (dt s)
            modifyIORef ioref (\x -> x { offsetY = (currentOffset + 0.03) :: GLfloat })
            --putStrLn "w"
        when (key == GLFW.Key'S) $ do
            r <- readIORef ioref
            s <- readIORef state
            let currentOffset = (offsetY r)
            let ct            = (dt s)
            modifyIORef ioref (\x -> x { offsetY = (currentOffset - 0.03) :: GLfloat })
            --putStrLn "s"
        when (key == GLFW.Key'U) $ do
            r <- readIORef ioref
            let currentScale = (scaleX r)
            modifyIORef ioref (\x -> x { scaleX = (currentScale * 1.5) :: GLfloat })
            --putStrLn "u"
        when (key == GLFW.Key'J) $ do
            r <- readIORef ioref
            let currentScale = (scaleX r)
            modifyIORef ioref (\x -> x { scaleX = (currentScale / 1.5) :: GLfloat })
            --putStrLn "j"
        when (key == GLFW.Key'F2) $ do
            r <- readIORef ioref
            let currentClamping = (clamping r)
            modifyIORef ioref (\x -> x { clamping = not currentClamping })
            putStrLn "f2"
        when (key == GLFW.Key'F3) $ do
            r <- readIORef ioref
            let currentInterpolation = (interpolate r)
            modifyIORef ioref (\x -> x { interpolate = not currentInterpolation })
            putStrLn "f3"
        when (key == GLFW.Key'F4) $ do
            r <- readIORef ioref
            let currentDraw = (drawpoints r)
            modifyIORef ioref (\x -> x { drawpoints = not currentDraw })
            putStrLn "f4"


cursorPosCallBack :: GLFW.CursorPosCallback
cursorPosCallBack window x y = do
    putStrLn $ "x: " ++ show x ++ ", y: " ++ show y
    return ()

framebufferSizeCallback _ width height = do
    let pos   = Position 0 0
        size  = Size (fromIntegral width) (fromIntegral height)
    viewport   $= (pos, size)

updateStateTime :: IORef State -> Double -> IO ()
updateStateTime s mt = do
    Just t <- liftIO $ GLFW.getTime
    state <- readIORef s
    let ct = convertFloat $ double2Float $ (t - mt) * 60
    modifyIORef s (\n -> n { dt = ct })


-------------------------------------------------------------------------------
-- Formula evaluation functions

generatePointsList formula xMin xMax yMin yMax
    = [ z | i <- [0..(gl_n - 1)]
          , j <- [0..(gl_n - 1)]
          , let x = mapRange (0, (gl_n - 1)) (xMin, xMax) j
          , let y = mapRange (0, (gl_n - 1)) (yMin, yMax) i
          , let Just z = evaluate (M.fromList [("x", x), ("y", y)]) . Just $ formula
      ]


boundsParser :: P.Parser Bound
boundsParser = do
    P.char '['
    var <- P.anyChar
    P.char ','
    P.skipSpace
    min <- P.number
    P.char ','
    P.skipSpace
    max <- P.number
    P.char ']'
    return $ Bound var min max

-- End of formula evaluation functions
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Main program loop

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    GLFW.windowHint $ GLFW.WindowHint'RefreshRate 60
    GLFW.windowHint $ GLFW.WindowHint'Resizable True
    GLFW.windowHint $ GLFW.WindowHint'AlphaBits 8
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
            (Just win) -> do
                GLFW.makeContextCurrent m
                f win
                GLFW.setErrorCallback $ Just simpleErrorCallback
                GLFW.destroyWindow win
            Nothing -> return ()
        GLFW.terminate
        exitSuccess
    where
        simpleErrorCallback e s =
            putStrLn $ unwords [show e, show s]


mainLoop :: GLFW.Window -> IORef Resources -> IORef State -> Double -> IO ()
mainLoop w r s mt = do
    clear [ColorBuffer, DepthBuffer]

    Just t <- liftIO $ GLFW.getTime
    
    updateStateTime s mt
    draw r s

    GLFW.swapBuffers w
    flush -- mb not needed
    GLFW.pollEvents

    q <- liftIO $ GLFW.windowShouldClose w
    unless q (mainLoop w r s t)


main :: IO ()
main = do
    args <- getArgs

    let
        Right parsedFormula = parse $ args !! 0

        -- "[x, -3, 3]"
        P.Done xi xresult@(Bound xVar xMin xMax) = P.parse boundsParser $ T.pack $ args !! 1
        -- "[y, -2, 2]"
        P.Done yi yresult@(Bound yVar yMin yMax) = P.parse boundsParser $ T.pack $ args !! 2 

        resultPointsList = toGraph $ generatePointsList parsedFormula
                                              (realToFrac xMin)
                                              (realToFrac xMax)
                                              (realToFrac yMin)
                                              (realToFrac yMax)


    withWindow windowWidth windowHeight "First window test :D" $ \win -> do
        clearColor $= Color4 0.0 0.0 0.0 1.0
        depthFunc  $= Just Less

        r <- (makeResources resultPointsList) >>= newIORef
        putStrLn "made resources"
        s <- makeState >>= newIORef
        drawInit r

        GLFW.setKeyCallback             win $ Just $ keyCallback             r s
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallBack
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback

        Just mt <- GLFW.getTime

        mainLoop win r s mt

    putStrLn "ended! :("

---  end of file
