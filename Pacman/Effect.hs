module Pacman.Effect (EffectId, EffectIdT(..), Effect(..), initialEffects) where

import Pacman.Actor

data EffectIdT a = PillPulsate | PacmanMouth | GhostFrill a | GhostLeftEye a | GhostRightEye a | PillFlash
type EffectId = EffectIdT GhostId

data Effect = Effect {
    effectId :: EffectId,
    effectValue :: Float,
    effectUpdate :: Float -> Effect -> Effect
    }

initialEffects ::  [Effect]
initialEffects = [pillPulsate]

pillPulsate ::  Effect
pillPulsate = Effect {
    effectId = PillPulsate,
    effectValue = 0,
    effectUpdate = update
} where
    update dt prev = Effect {
        effectId = PillPulsate,
        effectValue = cos (acos (effectValue prev) + dt),
        effectUpdate = update
    }
