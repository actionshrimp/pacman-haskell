module Pacman.Engine.Actor (actorsUpdate) where

import Data.List
import Data.Maybe

import Pacman.Util.Coords
import Pacman.Util.Route

import Pacman.Level
import Pacman.GameState
import Pacman.Actor

import Pacman.InputCommand

actorsUpdate :: Float -> Coords -> Level -> [GameState] -> [Actor] -> [Actor]
actorsUpdate dt inputPacDir level states actors = updatedActors where
    (activeActors, inactiveActors) = partition (actorIsActive states) actors
    targetDst a = (actorTargetDstFn a) inputPacDir actors a
    updatedActors = inactiveActors ++ map (\a -> actorUpdate dt level (targetDst a) a) activeActors

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

actorUpdate :: Float -> Level -> Coords -> Actor -> Actor
actorUpdate dt level targetDst a = Actor {
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
    newDst | newMoveParam < oldMoveParam && (length route > 1) = route !! 1
           | newMoveParam < oldMoveParam && (length route > 0) = route !! 0
           | otherwise = oldDst
    route = calculateActorRoute level a targetDst
    direcVec = direcVecCoords newSrc newDst
    levelW = levelWidth level
    --'Wrapped' functions allow the tunnel to work - the actor loops round in the X direction
    newDstWrapped | fst newSrc > levelW && (direcVec == (1, 0)) = (fst newDst - levelW, snd newDst)
                  | fst newSrc < 0 && (direcVec == (-1, 0)) = (fst newDst + levelW, snd newDst)
                  | otherwise = newDst
    newSrcWrapped | fst newSrc > levelW && (direcVec == (1, 0)) = (fst newSrc - levelW, snd newSrc)
                  | fst newSrc < 0 && (direcVec == (-1, 0))  = (fst newSrc + levelW, snd newDst)
                  | otherwise = newSrc
