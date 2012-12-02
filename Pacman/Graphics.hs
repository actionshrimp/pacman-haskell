module Pacman.Graphics (renderScene) where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix (ortho2D)

import Data.IORef
import qualified Data.Time.Clock as Clock

import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman

renderScene :: IORef Scene.Scene -> IO ()
renderScene sceneRef = do
    scene <- readIORef sceneRef
    clear [ColorBuffer]

    setProjection (Scene.width scene) (Scene.height scene)
    startDrawMode

    let pacman = Scene.pacman scene
    renderPacman (Pacman.position pacman) (Pacman.mouthAngle pacman) 

setProjection :: Int -> Int -> IO ()
setProjection width height = do
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral width) 0 (fromIntegral height)

startDrawMode :: IO ()
startDrawMode = do
    matrixMode $= (Modelview 0)
    loadIdentity

renderPacman :: (Float, Float) -> Float -> IO ()
renderPacman (x, y) mouthAngle = do
    let 
        r = 20
        points = [(x, y)] ++ filter (/= (x, y)) (map pacmanPoints [0, pi/64..2*pi])
        pacmanPoints angle | (acos $ cos angle) < mouthAngle = (x, y)
                           | otherwise = (x + r * cos angle, y + r * sin angle)
        pointToVertex (x, y) = (Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat)
        vertices = map pointToVertex points

    renderPrimitive TriangleFan $ do
        color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0 :: GLfloat)
        mapM_ vertex vertices
