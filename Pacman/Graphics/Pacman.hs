module Pacman.Graphics.Pacman (renderPacman) where

import Control.Arrow
import Graphics.Rendering.OpenGL

import Pacman.Graphics.Vertex
import Pacman.Graphics.Level

import Pacman.Util.Coords

import Pacman.World
import Pacman.Actor

import Pacman.Effects
import Pacman.Effects.PacmanMouthDirectionEffect
import Pacman.Effects.PacmanMouthChompEffect

renderPacman :: World -> IO ()
renderPacman world = do
    let 
        actors = worldActors world
        pacmanActor = actorWithId Pacman actors
        srcCoords = actorSrc pacmanActor
        dstCoords = actorDst pacmanActor
        direcCoords = direcVecCoords srcCoords dstCoords
        moveParam = actorMoveParam pacmanActor
        coords = translatePoint (scaleCoords moveParam direcCoords) (scaleCoords 1 srcCoords)
        (x, y) = scalePoint levelItemSize coords

        effects = worldEffects world
        direcEffect = pacmanMouthDirectionEffect effects
        directionAngle = pacmanMouthDirectionEffectAngleValue direcEffect

        chompEffect = pacmanMouthChompEffect effects
        chompAngle = pacmanMouthChompEffectAngle chompEffect

        mouthAngle = chompAngle

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
