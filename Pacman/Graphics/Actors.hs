module Pacman.Graphics.Actors (renderScene) where

import Graphics.Rendering.OpenGL

import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman

renderScene :: Scene.Scene -> IO ()
renderScene scene = do
    let pacman = Scene.pacman scene
    renderPacman (Pacman.position pacman) (Pacman.mouthAngle pacman) 

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
