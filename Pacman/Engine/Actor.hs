module Pacman.Engine.Actor (actorsUpdate) where

import Data.List
import Data.Maybe

import Pacman.Util.Coords
import Pacman.Util.Route

import Pacman.Level
import Pacman.GameState
import Pacman.Actor

actorsUpdate :: Float -> Coords -> Level -> [GameState] -> [Actor] -> [Actor]
actorsUpdate dt inputPacDir level states actors = updatedActors where
    (activeActors, inactiveActors) = partition (actorIsActive states) actors
    targetDst = actorTargetDst inputPacDir actors
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
    actorSrc = newSrc,
    actorDst = newDst,
    actorMoveParam = newMoveParam
} where
    oldMoveParam = actorMoveParam a
    newMoveParam | oldMoveParam >= 1 = oldMoveParam - 1
                 | otherwise = oldMoveParam + actorVelocity * dt
    oldSrc = actorSrc a
    oldDst = actorDst a
    newSrc | newMoveParam < oldMoveParam = oldDst
           | otherwise = oldSrc
    newDst | newMoveParam < oldMoveParam = nextSquare
           | otherwise = oldDst
    nextSquare = actorNextSquare level a targetSquare
    targetSquare = wrapAroundX level targetDst

actorNextSquare :: Level -> Actor -> Coords -> Coords
actorNextSquare level a target = nextSquare where
    nextSquare = if null calculatedRoute then nextSquareForNoRoute else head calculatedRoute
    calculatedRoute = fromMaybe [] (calculateActorRoute level a target)
    nextSquareForNoRoute = if isTraversable level nextSquareInSameDirection
                           then nextSquareInSameDirection
                           else actorDst a
    nextSquareInSameDirection = wrapAroundX level (translateCoords (actorDst a) direcVec)
    direcVec = direcVecCoords (actorSrc a) (actorDst a)

wrapAroundX :: Level -> Coords -> Coords
wrapAroundX level (x, y) | x >= w = (x - w, y)
                         | x < 0 = (x + w, y)
                         | otherwise = (x, y) 
                         where w = levelWidth level
