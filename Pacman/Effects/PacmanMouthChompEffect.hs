module Pacman.Effects.PacmanMouthChompEffect (PacmanMouthChompEffect(..), pacmanMouthChompEffectUpdate) where

import Pacman.Actor

data PacmanMouthChompEffect = PacmanMouthChompEffect {
    pacmanMouthChompEffectAngle :: Float,
    pacmanMouthChompEffectMaxAngle :: Float,
    pacmanMouthChompEffectVelocity :: Float,
    pacmanMouthChompEffectMoveParamStore :: Float,
    pacmanMouthChompEffectDirection :: Float
}

pacmanMouthChompEffectUpdate :: [Actor] -> Float -> PacmanMouthChompEffect -> PacmanMouthChompEffect
pacmanMouthChompEffectUpdate actors dt prev = PacmanMouthChompEffect {
    pacmanMouthChompEffectVelocity = v,
    pacmanMouthChompEffectAngle = newAngle,
    pacmanMouthChompEffectMaxAngle = maxAngle,
    pacmanMouthChompEffectMoveParamStore = newMoveParam,
    pacmanMouthChompEffectDirection = chompDirection
} where
    v = pacmanMouthChompEffectVelocity prev
    maxAngle = pacmanMouthChompEffectMaxAngle prev
    pacman = actorWithId Pacman actors
    lastMoveParam = pacmanMouthChompEffectMoveParamStore prev
    newMoveParam = actorMoveParam pacman
    moving = actorSrc pacman /= actorDst pacman
    prevAngle = pacmanMouthChompEffectAngle prev
    prevChompDirection = pacmanMouthChompEffectDirection prev
    chompDirection | iterAngle >= maxAngle = -1
                   | iterAngle <= 0 = 1
                   | otherwise = prevChompDirection
    iterAngle = prevAngle + v * dt * prevChompDirection
    newAngle | moving && iterAngle > maxAngle = maxAngle - (iterAngle - maxAngle)
             | moving && iterAngle < 0 = negate iterAngle
             | moving = iterAngle
             | otherwise = maxAngle / 2
