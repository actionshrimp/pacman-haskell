module Pacman.Graphics.Pacman (renderPacman) where

import Graphics.Rendering.OpenGL

import Pacman.Graphics.Base

import Pacman.Util.Types.Direction

import qualified Pacman.Actors.Types.Pacman as Pacman

directionAngle :: Floating a => Direction -> a
directionAngle DRight = 0
directionAngle DUp = pi / 2
directionAngle DLeft = pi
directionAngle DDown = 3 * pi / 2

renderPacman :: Pacman.Pacman -> IO ()
renderPacman pacman = do
    let 
        (x, y) = Pacman.position pacman
        oldD = Pacman.prevDirection pacman
        newD = Pacman.direction pacman
        oldDA = directionAngle oldD
        newDA = directionAngle newD
        deltaDA | (oldD == DRight && newD == DDown) = - pi / 2
                | (oldD == DDown && newD == DRight) = pi / 2
                | otherwise = newDA - oldDA
        d = oldDA + deltaDA * (Pacman.directionChangeParam pacman)
        mouthAngle = Pacman.mouthAngle pacman
        r = 20
        points = (x, y) : (map pacmanPoints [0, pi/64..2*pi])
        pacmanPoints angle | (acos . cos $ (angle - d)) < mouthAngle = (x, y)
                           | otherwise = (x + r * cos angle, y + r * sin angle)
        vertices = map pointToVertex points

        yellow = Color3 1 1 0 :: Color3 GLfloat

    renderPrimitive TriangleFan $ do
        color yellow
        mapM_ vertex vertices
