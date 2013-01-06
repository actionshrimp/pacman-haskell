module Pacman.Engine.Actor (actorsUpdate) where

import qualified Debug.Trace as T

import Data.List

import Pacman.Util.Coords
import Pacman.Util.Route

import Pacman.Level
import Pacman.GameState
import Pacman.Actor

actorsUpdate :: Float -> Coords -> Level -> [GameState] -> [Actor] -> [Actor]
actorsUpdate dt inputPacDir level states actors = updatedActors where
    (activeActors, inactiveActors) = partition (actorIsActive states) actors
    updatedActors = inactiveActors ++ map (\a -> actorUpdate dt level (targetSquare a) a) activeActors
    targetSquare = actorTargetSquare level states actors inputPacDir


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
actorUpdate dt level targetSquare a = Actor {
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
    newDst | newMoveParam < oldMoveParam = targetSquare
           | otherwise = oldDst

actorTargetSquare :: Level -> [GameState] -> [Actor] -> Coords -> Actor -> Coords
actorTargetSquare level states actors inputCoords a | actorId a == Pacman = pacmanTarget
                                                    | otherwise = head (calculateActorRoute level a ghostTarget) where
                                                        pacmanTarget = playerTargetSquare level inputCoords a
                                                        ghostTarget = ghostTargetDst isScattering isReturningHome (levelWidth level) (levelHeight level) actors a
                                                        Ghost gId = actorId a
                                                        isScattering = any (\s -> gameStateId s == GhostScattering gId) states
                                                        isReturningHome = any (\s -> gameStateId s == GhostReturningHome gId) states

playerTargetSquare :: Level -> Coords -> Actor -> Coords
playerTargetSquare level inputCoords actor | isTraversable level new = new
                                           | isTraversable level sameDir = sameDir
                                           | otherwise = current where
                                            current = actorDst actor
                                            sameDir = wrapAroundX level $ translateCoords current (direcVecCoords (actorSrc actor) current)
                                            new = wrapAroundX level $ translateCoords current inputCoords

wrapAroundX :: Level -> Coords -> Coords
wrapAroundX level (x, y) | x >= w = (x - w, y)
                         | x < 0 = (x + w, y)
                         | otherwise = (x, y) 
                         where w = levelWidth level
