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
    moved = newMoveParam /= lastMoveParam
    prevAngle = pacmanMouthChompEffectAngle prev
    prevChompDirection = pacmanMouthChompEffectDirection prev
    chompDirection | prevAngle > maxAngle = -1
                   | prevAngle < 0 = 1
                   | otherwise = prevChompDirection
    newAngle | moved = prevAngle + v * dt * chompDirection
             | otherwise = prevAngle
