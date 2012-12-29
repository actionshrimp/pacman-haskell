module Pacman.Effects (Effects(..), PillPulsateEffect(..), PacmanMouthDirectionEffect(..), initialEffects, effectsUpdate) where

import Pacman.Util.Coords

import Pacman.Actor

data Effects = Effects {
    pillPulsateEffect :: PillPulsateEffect,
    pacmanMouthDirectionEffect :: PacmanMouthDirectionEffect
}

effectsUpdate ::  [Actor] -> Float -> Effects -> Effects
effectsUpdate actors dt effects = Effects {
    pillPulsateEffect = pillPulsateEffectUpdate dt (pillPulsateEffect effects),
    pacmanMouthDirectionEffect = pacmanMouthDirectionEffectUpdate actors dt (pacmanMouthDirectionEffect effects)
}

initialEffects ::  Effects
initialEffects = Effects {
    pillPulsateEffect = PillPulsateEffect {
        pillPulsateEffectValue = 0,
        pillPulsateEffectParam = 0,
        pillPulsateEffectVelocity = 5
    },
    pacmanMouthDirectionEffect = PacmanMouthDirectionEffect {
        pacmanMouthDirectionEffectAngleValue = 0,
        pacmanMouthDirectionEffectRotationVelocity = 1,
        pacmanMouthDirectionEffectTargetDirection = (1, 0),
        pacmanMouthDirectionEffectLastDirection = (1, 0),
        pacmanMouthDirectionEffectParam = 1
    }
}

data PillPulsateEffect = PillPulsateEffect {
    pillPulsateEffectValue :: Float,
    pillPulsateEffectParam :: Float,
    pillPulsateEffectVelocity :: Float
}

pillPulsateEffectUpdate :: Float -> PillPulsateEffect -> PillPulsateEffect
pillPulsateEffectUpdate dt prev = PillPulsateEffect {
        pillPulsateEffectValue = sin newParam,
        pillPulsateEffectParam = newParam,
        pillPulsateEffectVelocity = v
    } where
        oldParam = pillPulsateEffectParam prev
        v = pillPulsateEffectVelocity prev
        newParam | oldParam  >= (2 * pi) = oldParam - 2 * pi
                 | otherwise = oldParam + v * dt

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
        pacmanDirecVec = direcVecCoords (actorDst pacman) (actorSrc pacman)
        prevTargetDirection = pacmanMouthDirectionEffectTargetDirection prev
        prevLastDirection = pacmanMouthDirectionEffectLastDirection prev
        prevParamValue = pacmanMouthDirectionEffectParam prev
        v = pacmanMouthDirectionEffectRotationVelocity prev
        newTargetDirection | pacmanDirecVec == (0, 0) = prevTargetDirection
                           | otherwise = pacmanDirecVec
        newLastDirection | newTargetDirection == prevTargetDirection = prevLastDirection
                         | otherwise = prevTargetDirection
        newParamValue | newTargetDirection == prevTargetDirection = max 1 (prevParamValue + v * dt)
                      | otherwise = 0
        targetScaled = scaleCoords newParamValue newTargetDirection
        lastScaled = scaleCoords (1 - newParamValue) newLastDirection
        angleDirecVec = (fst targetScaled + fst lastScaled, snd targetScaled + snd lastScaled)
        --angleDirecVec = (targetScaled *** ((+), (+))) *** lastScaled -- why doesnt this work?
        angle = atan (snd angleDirecVec / fst angleDirecVec)
