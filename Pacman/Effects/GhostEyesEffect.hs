module Pacman.Effects.GhostEyesEffect (GhostEyesEffect(..), ghostEyesEffectUpdate) where

import Pacman.Util.Coords

import Pacman.Actor

data GhostEyesEffect = GhostEyesEffect {
    ghostEyesEffectGhostId :: GhostId,
    ghostEyesEffectVelocity :: Float, --3
    ghostEyesEffectPosition :: (Float, Float)
}

ghostEyesEffectUpdate :: [Actor] -> Float -> GhostEyesEffect -> GhostEyesEffect
ghostEyesEffectUpdate actors dt prev = GhostEyesEffect {
    ghostEyesEffectGhostId = gId,
    ghostEyesEffectVelocity = v,
    ghostEyesEffectPosition = newPosition
} where
    v = ghostEyesEffectVelocity prev
    gId = ghostEyesEffectGhostId prev
    ghost = actorWithId (Ghost gId) actors
    s = actorSrc ghost
    d = actorDst ghost
    (baseDirecX, baseDirecY) = direcVecCoords s d
    --Get direction accounting for wrapping around the X axis
    (direcX, direcY) | abs (fst d - fst s) > 1 = (negate baseDirecX `div` abs baseDirecX, baseDirecY)
                     | otherwise = (baseDirecX, baseDirecY)
    (targetX, targetY) = (fromIntegral direcX, fromIntegral direcY)
    (curX, curY) = ghostEyesEffectPosition prev
    vX = v * (targetX - curX)
    vY = v * (targetY - curY)
    newPosition = (curX + vX * dt, curY + vY * dt)
