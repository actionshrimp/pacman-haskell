module Pacman.Effects.GhostFrillEffect (GhostFrillEffect(..), ghostFrillEffectUpdate) where

data GhostFrillEffect = GhostFrillEffect {
    ghostFrillEffectValue :: Float,
    ghostFrillEffectVelocity :: Float
}

ghostFrillEffectUpdate :: Float -> GhostFrillEffect -> GhostFrillEffect
ghostFrillEffectUpdate dt prev = GhostFrillEffect {
    ghostFrillEffectVelocity = v,
    ghostFrillEffectValue = newValue

} where
    v = ghostFrillEffectVelocity prev
    prevValue = ghostFrillEffectValue prev
    newValueAttempt = prevValue + v * dt
    newValue | newValueAttempt > 1 = newValueAttempt - 1
             | otherwise = newValueAttempt
