module Pacman.Graphics (initializeGraphics, renderLoop) where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix (ortho2D)
import Graphics.UI.GLUT
import Data.IORef

import Pacman.Actors

initializeGraphics :: IORef Scene -> IO ()
initializeGraphics sceneRef = do
    scene <- readIORef sceneRef
    setWindowOptions (width scene) (height scene)
    (progname, _) <- getArgsAndInitialize
    createWindow progname
    displayCallback $= (display sceneRef)

renderLoop :: IO ()
renderLoop = do
    mainLoop

setWindowOptions :: Int -> Int -> IO ()
setWindowOptions width height = do
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    initialDisplayMode $= [WithSamplesPerPixel 4] --AA

display :: IORef Scene -> IO ()
display sceneRef = do
    scene <- readIORef sceneRef
    clear [ColorBuffer]

    setProjection (width scene) (height scene)
    startDrawMode

    renderPacman (position . pacman $ scene)

    flush

setProjection :: Int -> Int -> IO ()
setProjection width height = do
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral width) 0 (fromIntegral height)

startDrawMode :: IO ()
startDrawMode = do
    matrixMode $= (Modelview 0)
    loadIdentity

renderPacman :: (Float, Float) -> IO ()
renderPacman (x, y) = do
    let 
        r = 200
        points = [(x, y)] ++ map pacmanPoints [0, pi/16..2*pi]
        pacmanPoints angle | (acos $ cos angle) < (pi/8) = (x, y)
                           | otherwise = (x + r * cos angle, y + r * sin angle)
        pointToVertex (x, y) = (Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat)
        vertices = map pointToVertex points

    renderPrimitive TriangleFan $ do
        color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)
        mapM_ vertex vertices
