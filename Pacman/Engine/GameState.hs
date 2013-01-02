module Pacman.Engine.GameState (statesUpdate) where

import Data.List

import Pacman.Level
import Pacman.GameState

statesUpdate :: Level -> Float -> [GameState] -> [GameState]
statesUpdate level dt states = finalStates where
    timeUpdatedStates = updateStateTimes $ states
    updateStateTimes = map (\s -> GameState { gameStateId = gameStateId s, gameStateRemainingTime = (gameStateRemainingTime s - dt) })
    (finishedStates, tickingStates) = partition (\s -> gameStateRemainingTime s < 0) timeUpdatedStates
    finishedStateIds = map gameStateId finishedStates

    waitingToActivating (GhostWaiting gId) = [GameState { gameStateId = GhostActivating gId, gameStateRemainingTime = 0.75 }]
    waitingToActivating _ = []

    activating = concatMap waitingToActivating finishedStateIds

    finalStates = tickingStates ++ activating
