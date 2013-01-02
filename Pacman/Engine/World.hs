module Pacman.Engine.World (worldUpdate) where

import Pacman.Util.Coords

import Pacman.World

import Pacman.Engine.Level
import Pacman.Engine.Actor
import Pacman.Engine.Effects
import Pacman.Engine.GameState

import Pacman.InputCommand

worldUpdate :: Float -> Maybe InputCommand -> World -> World
worldUpdate dt input world = World {
    worldElapsedTime = worldElapsedTime world + dt,
    worldActors = actorsUpdate dt inputPacDir level states actors,
    worldEffects = effectsUpdate actors dt effects,
    worldStates = statesUpdate level dt states,
    worldLevel = newLevel,
    worldPacmanInputDirection = inputPacDir
} where 
    states = worldStates world
    effects = worldEffects world
    actors = worldActors world
    level = worldLevel world
    newLevel = levelUpdate actors level
    prevPacDir = worldPacmanInputDirection world
    inputPacDir = pacmanDirectionFromInput prevPacDir input

pacmanDirectionFromInput :: Coords -> Maybe InputCommand -> Coords
pacmanDirectionFromInput _ (Just MovePacmanUp) = (0, 1)
pacmanDirectionFromInput _ (Just MovePacmanDown) = (0, -1)
pacmanDirectionFromInput _ (Just MovePacmanLeft) = (-1, 0)
pacmanDirectionFromInput _ (Just MovePacmanRight) = (1, 0)
pacmanDirectionFromInput prev _ = prev
