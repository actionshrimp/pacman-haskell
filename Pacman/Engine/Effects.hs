module Pacman.Engine.Effects (effectsUpdate) where

import Pacman.Actor
import Pacman.Effects

import Pacman.Effects.PillPulsateEffect
import Pacman.Effects.PacmanMouthDirectionEffect
import Pacman.Effects.PacmanMouthChompEffect
import Pacman.Effects.GhostFrillEffect
import Pacman.Effects.GhostEyesEffect

effectsUpdate ::  [Actor] -> Float -> Effects -> Effects
effectsUpdate actors dt effects = Effects {
    pillPulsateEffect = pillPulsateEffectUpdate dt (pillPulsateEffect effects),
    pacmanMouthDirectionEffect = pacmanMouthDirectionEffectUpdate actors dt (pacmanMouthDirectionEffect effects),
    pacmanMouthChompEffect = pacmanMouthChompEffectUpdate actors dt (pacmanMouthChompEffect effects),
    ghostFrillEffect = ghostFrillEffectUpdate dt (ghostFrillEffect effects),
    ghostEyesEffects = map (ghostEyesEffectUpdate actors dt) (ghostEyesEffects effects)
}

