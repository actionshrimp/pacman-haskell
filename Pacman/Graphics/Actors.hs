module Pacman.Graphics.Actors (renderScene) where

import Graphics.Rendering.OpenGL

import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman
import qualified Pacman.Actors.Ghost as Ghost

renderScene :: Scene.Scene -> IO ()
renderScene scene = do
    renderPacman (Scene.pacman scene)
    mapM_ (\(i, ghost) -> renderGhost i (ghost)) (zip [1..] (Scene.ghosts scene))

pointToVertex :: (Float, Float) -> Vertex3 GLfloat
pointToVertex (x, y) = (Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat)

renderPacman :: Pacman.Pacman -> IO ()
renderPacman pacman = do
    let 
        (x, y) = Pacman.position pacman
        mouthAngle = Pacman.mouthAngle pacman
        r = 20
        points = [(x, y)] ++ filter (/= (x, y)) (map pacmanPoints [0, pi/64..2*pi])
        pacmanPoints angle | (acos $ cos angle) < mouthAngle = (x, y)
                           | otherwise = (x + r * cos angle, y + r * sin angle)
        vertices = map pointToVertex points

    renderPrimitive TriangleFan $ do
        color $ (Color3 1 1 0 :: Color3 GLfloat)
        mapM_ vertex vertices

renderGhost :: Int -> Ghost.Ghost -> IO ()
renderGhost index ghost = do
    let
        (x, y) = Ghost.position ghost
        r = 20
        points = [(x, y)] ++ map (\a -> (x + r * cos a, y + r * sin a)) [0, pi/64..pi]
        vertices = map pointToVertex points
        drawColor = case index of 
            1 -> (Color3 1  0   0 :: Color3 GLfloat)
            2 -> (Color3 1  0.4 1 :: Color3 GLfloat)
            3 -> (Color3 0  1   1 :: Color3 GLfloat)
            4 -> (Color3 1  0.6 0 :: Color3 GLfloat)

    renderPrimitive TriangleFan $ do
        color drawColor
        mapM_ vertex vertices
