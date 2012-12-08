module Pacman.Graphics.Pacman (renderPacman) where

import Graphics.Rendering.OpenGL

import Pacman.Graphics.Base
import qualified Pacman.Actors.Pacman as Pacman

renderPacman :: Pacman.Pacman -> IO ()
renderPacman pacman = do
    let 
        (x, y) = Pacman.position pacman
        mouthAngle = Pacman.mouthAngle pacman
        r = 20
        points = (x, y) : filter (/= (x, y)) (map pacmanPoints [0, pi/64..2*pi])
        pacmanPoints angle | (acos . cos $ angle) < mouthAngle = (x, y)
                           | otherwise = (x + r * cos angle, y + r * sin angle)
        vertices = map pointToVertex points

        yellow = Color3 1 1 0 :: Color3 GLfloat

    renderPrimitive TriangleFan $ do
        color yellow
        mapM_ vertex vertices
