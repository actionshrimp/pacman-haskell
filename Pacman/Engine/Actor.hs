module Pacman.Engine.Actor (actorsUpdate) where

import Data.List
import Data.Maybe

import Pacman.Util.Coords

import Pacman.Level
import Pacman.GameState
import Pacman.Actor

import Pacman.InputCommand

actorsUpdate :: Float -> Coords -> Level -> [GameState] -> [Actor] -> [Actor]
actorsUpdate dt inputPacDir level states actors = updatedActors where
    (activeActors, inactiveActors) = partition (actorIsActive states) actors
    targetDst a = (actorTargetDstFn a) inputPacDir actors level a
    levelDimensions = (levelWidth level, levelHeight level)
    updatedActors = inactiveActors ++ map (\a -> actorUpdate dt levelDimensions (targetDst a) a) activeActors

actorIsActive :: [GameState] -> Actor -> Bool
actorIsActive states actor = not (isWaitingGhost states aId) && not (isActivatingGhost states aId) where
    aId = actorId actor

isWaitingGhost :: [GameState] -> ActorId -> Bool
isWaitingGhost states (Ghost gId) = (GhostWaiting gId) `elem` stateIds where
    stateIds = map gameStateId states
isWaitingGhost _ _ = False

isActivatingGhost :: [GameState] -> ActorId -> Bool
isActivatingGhost states (Ghost gId) = (GhostActivating gId) `elem` stateIds where
    stateIds = map gameStateId states
isActivatingGhost _ _ = False

actorUpdate :: Float -> Coords -> Coords -> Actor -> Actor
actorUpdate dt levelSize targetDst a = Actor {
    actorId = actorId a,
    actorSrc = newSrcWrapped,
    actorDst = newDstWrapped,
    actorMoveParam = newMoveParam,
    actorTargetDstFn = actorTargetDstFn a
} where
    oldMoveParam = actorMoveParam a
    newMoveParam | (targetDst == oldDst) && (oldMoveParam >= 1) = oldMoveParam
                 | oldMoveParam >= 1 = oldMoveParam - 1
                 | otherwise = oldMoveParam + actorVelocity * dt
    oldSrc = actorSrc a
    oldDst = actorDst a
    newSrc | newMoveParam < oldMoveParam = oldDst
           | otherwise = oldSrc
    newDst | newMoveParam < oldMoveParam = targetDst
           | otherwise = oldDst
    direcVec = direcVecCoords newSrc newDst
    --'Wrapped' functions allow the tunnel to work - the actor loops round in the X direction
    newDstWrapped | fst newSrc > fst levelSize && (direcVec == (1, 0)) = (fst newDst - (fst levelSize + 2), snd newDst)
                  | fst newSrc < 0 && (direcVec == (-1, 0)) = (fst newDst + (fst levelSize + 2), snd newDst)
                  | otherwise = newDst
    newSrcWrapped | fst newSrc > fst levelSize && (direcVec == (1, 0)) = (fst newSrc - (fst levelSize + 2), snd newSrc)
                  | fst newSrc < 0 && (direcVec == (-1, 0))  = (fst newSrc + (fst levelSize + 2), snd newDst)
                  | otherwise = newSrc
