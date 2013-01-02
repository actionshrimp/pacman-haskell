module Pacman.GameState (GameStateId, GameStateIdT(..), GameState(..), initialGameStates, zeroTimeState) where

import Pacman.Actor

data GameStateIdT a = Starting | PacmanDead | GhostWaiting a | GhostActivating a | GhostReturningHome a | GhostsFleeing | ZeroTime deriving (Eq)

type GameStateId = GameStateIdT GhostId

data GameState = GameState {
    gameStateId :: GameStateId,
    gameStateRemainingTime :: Float
}

initialGameStates ::  [GameState]
initialGameStates = [
    GameState {
        gameStateId = GhostWaiting GhostB,
        gameStateRemainingTime = 10
        },
    GameState {
        gameStateId = GhostWaiting GhostC,
        gameStateRemainingTime = 20
        },
    GameState {
        gameStateId = GhostWaiting GhostD,
        gameStateRemainingTime = 30
        }
    ]

zeroTimeState ::  GameState
zeroTimeState = GameState {
    gameStateId = ZeroTime,
    gameStateRemainingTime = 0
}
