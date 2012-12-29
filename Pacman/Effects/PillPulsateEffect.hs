module Pacman.Effects.PillPulsateEffect (PillPulsateEffect(..), pillPulsateEffectUpdate) where

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
