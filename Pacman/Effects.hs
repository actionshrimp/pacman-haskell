module Pacman.Effects (Effects(..), initialEffects, effectsUpdate) where

import Pacman.Actor

import Pacman.Effects.PillPulsateEffect
import Pacman.Effects.PacmanMouthDirectionEffect
import Pacman.Effects.PacmanMouthChompEffect
import Pacman.Effects.GhostFrillEffect
import Pacman.Effects.GhostEyesEffect

data Effects = Effects {
    pillPulsateEffect :: PillPulsateEffect,
    pacmanMouthDirectionEffect :: PacmanMouthDirectionEffect,
    pacmanMouthChompEffect :: PacmanMouthChompEffect,
    ghostFrillEffect :: GhostFrillEffect,
    ghostEyesEffects :: [GhostEyesEffect]
}

effectsUpdate ::  [Actor] -> Float -> Effects -> Effects
effectsUpdate actors dt effects = Effects {
    pillPulsateEffect = pillPulsateEffectUpdate dt (pillPulsateEffect effects),
    pacmanMouthDirectionEffect = pacmanMouthDirectionEffectUpdate actors dt (pacmanMouthDirectionEffect effects),
    pacmanMouthChompEffect = pacmanMouthChompEffectUpdate actors dt (pacmanMouthChompEffect effects),
    ghostFrillEffect = ghostFrillEffectUpdate dt (ghostFrillEffect effects),
    ghostEyesEffects = map (ghostEyesEffectUpdate actors dt) (ghostEyesEffects effects)
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
    },
    ghostFrillEffect = GhostFrillEffect {
        ghostFrillEffectVelocity = 0.5,
        ghostFrillEffectValue = 0
    },
    ghostEyesEffects = [
        GhostEyesEffect {
            ghostEyesEffectGhostId = GhostA,
            ghostEyesEffectVelocity = 3,
            ghostEyesEffectPosition = (0, 0)
        },
        GhostEyesEffect {
            ghostEyesEffectGhostId = GhostB,
            ghostEyesEffectVelocity = 3,
            ghostEyesEffectPosition = (0, 0)
        },
        GhostEyesEffect {
            ghostEyesEffectGhostId = GhostC,
            ghostEyesEffectVelocity = 3,
            ghostEyesEffectPosition = (0, 0)
        },
        GhostEyesEffect {
            ghostEyesEffectGhostId = GhostD,
            ghostEyesEffectVelocity = 3,
            ghostEyesEffectPosition = (0, 0)
        }
    ]
}
