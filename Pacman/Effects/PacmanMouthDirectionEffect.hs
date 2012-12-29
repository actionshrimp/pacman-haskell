module Pacman.Effects.PacmanMouthDirectionEffect (PacmanMouthDirectionEffect(..), pacmanMouthDirectionEffectUpdate) where

import Pacman.Util.Coords

import Pacman.Actor

data PacmanMouthDirectionEffect = PacmanMouthDirectionEffect {
    pacmanMouthDirectionEffectAngleValue :: Float,
    pacmanMouthDirectionEffectRotationVelocity :: Float,
    pacmanMouthDirectionEffectTargetDirection :: Coords,
    pacmanMouthDirectionEffectLastDirection :: Coords,
    pacmanMouthDirectionEffectParam :: Float
}

pacmanMouthDirectionEffectUpdate :: [Actor]-> Float-> PacmanMouthDirectionEffect-> PacmanMouthDirectionEffect
pacmanMouthDirectionEffectUpdate actors dt prev = PacmanMouthDirectionEffect {
        pacmanMouthDirectionEffectAngleValue = angle,
        pacmanMouthDirectionEffectRotationVelocity = v,
        pacmanMouthDirectionEffectTargetDirection = newTargetDirection,
        pacmanMouthDirectionEffectLastDirection = newLastDirection,
        pacmanMouthDirectionEffectParam = newParamValue
    } where
        pacman = actorWithId Pacman actors
        pacmanDirecVec = direcVecCoords (actorSrc pacman) (actorDst pacman)
        prevTargetDirection = pacmanMouthDirectionEffectTargetDirection prev
        prevLastDirection = pacmanMouthDirectionEffectLastDirection prev
        prevParamValue = pacmanMouthDirectionEffectParam prev
        v = pacmanMouthDirectionEffectRotationVelocity prev
        newTargetDirection | pacmanDirecVec == (0, 0) = prevTargetDirection
                           | otherwise = pacmanDirecVec
        newLastDirection | newTargetDirection == prevTargetDirection = prevLastDirection
                         | otherwise = prevTargetDirection
        newParamValue | newTargetDirection == prevTargetDirection = min 1 (prevParamValue + v * dt)
                      | otherwise = 0
        targetAngle = atan2 (fromIntegral . snd $ newTargetDirection) (fromIntegral . fst $ newTargetDirection)
        lastAngle = atan2 (fromIntegral . snd $ newLastDirection) (fromIntegral . fst $ newLastDirection)
        angle = lastAngle + newParamValue * (targetAngle - lastAngle)
