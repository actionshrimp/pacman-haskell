module Pacman.Engine (updateWorld) where

import qualified Data.Map as M

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
    worldLevel = newLevel
} where 
    actors = worldActors world
    level = worldLevel world
    targetDst a = (actorTargetDstFn a) input actors level a
    levelDimensions = (levelWidth level, levelHeight level)
    newLevel = updateLevel actors level

updateLevel :: [Actor] -> Level -> Level
updateLevel actors prev = Level {
    levelWidth = levelWidth prev,
    levelHeight = levelHeight prev,
    levelItems = newLevelItems
} where
    pacman = actorWithId Pacman actors
    pacmanPos = actorSrc pacman
    newLevelItems = M.adjust (\_ -> Blank) pacmanPos (levelItems prev)
