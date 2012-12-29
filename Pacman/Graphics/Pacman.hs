module Pacman.Graphics.Pacman (renderPacman) where

import Control.Arrow
import Graphics.Rendering.OpenGL

import Pacman.Graphics.Vertex
import Pacman.Graphics.Level

import Pacman.Util.Coords

import Pacman.World
import Pacman.Actor
import Pacman.Effects

deriveDirectionAngle :: Coords -> Float
deriveDirectionAngle (1, 0) = 0
deriveDirectionAngle (0, 1) = pi / 2
deriveDirectionAngle (-1, 0) = pi
deriveDirectionAngle (0, -1) = 3 * pi / 2
deriveDirectionAngle _ = 0

renderPacman :: World -> IO ()
renderPacman world = do
    let 
        actors = worldActors world
        pacmanActor = actorWithId Pacman actors
        srcCoords = actorSrc pacmanActor
        dstCoords = actorDst pacmanActor
        direcCoords = translateCoords dstCoords (negateCoords srcCoords)
        moveParam = actorMoveParam pacmanActor
        coords = translatePoint (scaleCoords moveParam direcCoords) (scaleCoords 1 srcCoords)
        (x, y) = scalePoint levelItemSize coords

        --oldD = Pacman.prevDirection pacman
        --newD = Pacman.direction pacman
        --oldDA = directionAngle oldD
        --newDA = directionAngle newD
        --deltaDA | (oldD == DRight && newD == DDown) = - pi / 2
        --        | (oldD == DDown && newD == DRight) = pi / 2
        --        | otherwise = newDA - oldDA
        --d = oldDA + deltaDA * (Pacman.directionChangeParam pacman)
        --mouthAngle = Pacman.mouthAngle pacman

        directionAngle = deriveDirectionAngle (direcVecCoords (actorSrc pacmanActor) (actorDst pacmanActor))
        mouthAngle = pi / 8

        r = 0.75
        points = (0, 0) : (map pacmanPoints [0, pi/64..2*pi])
        pacmanPoints angle | (acos . cos $ (angle - directionAngle)) < mouthAngle = (0, 0)
                           | otherwise = (r * cos angle, r * sin angle)

        midPositionedPoints = map (translatePoint (0.5, 0.5)) points
        scaledPoints = map (scalePoint levelItemSize) midPositionedPoints
        scaledPositionedPoints = map (translatePoint (x, y)) scaledPoints

        vertices = map pointToVertex scaledPositionedPoints

        yellow = Color3 1 1 0 :: Color3 GLfloat

    renderPrimitive TriangleFan $ do
        color yellow
        mapM_ vertex vertices
