module Pacman.Engine (updateWorld) where

import Pacman.World
import Pacman.Level
import Pacman.Effects
import Pacman.Actor

import Pacman.InputCommand

updateWorld :: Float -> Maybe InputCommand -> World -> World
updateWorld dt input world = World {
    worldElapsedTime = worldElapsedTime world + dt,
    worldActors = map (\a -> actorUpdate dt levelDimensions (targetDst a) a) actors,
    worldEffects = effectsUpdate actors dt (worldEffects world),
    worldStates = worldStates world,
    worldLevel = level
} where 
    actors = worldActors world
    level = worldLevel world
    targetDst a = (actorTargetDstFn a) input actors level a
    levelDimensions = (levelWidth level, levelHeight level)
