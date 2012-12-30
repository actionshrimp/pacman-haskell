module Pacman.Effects (Effects(..), initialEffects, effectsUpdate) where

import Pacman.Actor

import Pacman.Effects.PillPulsateEffect
import Pacman.Effects.PacmanMouthDirectionEffect
import Pacman.Effects.PacmanMouthChompEffect

data Effects = Effects {
    pillPulsateEffect :: PillPulsateEffect,
    pacmanMouthDirectionEffect :: PacmanMouthDirectionEffect,
    pacmanMouthChompEffect :: PacmanMouthChompEffect
}

effectsUpdate ::  [Actor] -> Float -> Effects -> Effects
effectsUpdate actors dt effects = Effects {
    pillPulsateEffect = pillPulsateEffectUpdate dt (pillPulsateEffect effects),
    pacmanMouthDirectionEffect = pacmanMouthDirectionEffectUpdate actors dt (pacmanMouthDirectionEffect effects),
    pacmanMouthChompEffect = pacmanMouthChompEffectUpdate actors dt (pacmanMouthChompEffect effects) 
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
        pacmanMouthDirectionEffectRotationVelocity = 8,
        pacmanMouthDirectionEffectTargetDirection = (1, 0),
        pacmanMouthDirectionEffectLastDirection = (1, 0),
        pacmanMouthDirectionEffectParam = 1
    },
    pacmanMouthChompEffect = PacmanMouthChompEffect {
        pacmanMouthChompEffectAngle = 0,
        pacmanMouthChompEffectMaxAngle = pi / 4,
        pacmanMouthChompEffectVelocity = (pi / 2) * actorVelocity,
        pacmanMouthChompEffectMoveParamStore = 0,
        pacmanMouthChompEffectDirection = 1
    }
}
