module Pacman.World (World(..), initializeWorld) where

import Pacman.Actor
import Pacman.Level
import Pacman.Effects
import Pacman.GameState

data World = World {
    worldElapsedTime :: Float,
    worldActors :: [Actor], --Actors moving around in the world
    worldEffects :: Effects, -- Effects (params used mostly for animation)
    worldStates :: [GameState], -- Transient gameplay states (gate open/closed, ghosts fleeing etc.)
    worldLevel :: Level -- The static level items (which actors can potentially interact with)
}

initializeWorld ::  [String] -> World
initializeWorld levelData = World {
    worldElapsedTime = 0.0,
    worldActors = initialActors levelData,
    worldEffects = initialEffects,
    worldStates = initialGameStates,
    worldLevel = loadLevel levelData
}
