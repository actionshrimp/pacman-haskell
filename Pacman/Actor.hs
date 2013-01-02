module Pacman.Actor (ActorId, ActorIdT(..), GhostId(..), Actor(..), initialActors, actorWithId, actorVelocity) where

import Data.List
import qualified Data.Map as M
import Data.Maybe

import Pacman.Util.Coords
import Pacman.InputCommand

import Pacman.Level

data ActorIdT a = Pacman | Ghost a deriving (Eq)
data GhostId = GhostA | GhostB | GhostC | GhostD deriving (Eq)

type ActorId = ActorIdT GhostId

--An actor is moving from coord Src to coord Dst.
--The MoveParam is how far from Src to Dst they have reached (0.5 is halfway there).
--Src and Dst coords are right next to each other
data Actor = Actor {
    actorId :: ActorId,
    actorSrc :: Coords, 
    actorDst :: Coords,
    actorMoveParam :: Float,
    actorTargetDstFn :: Coords -> [Actor] -> Level -> Actor -> Coords
}

actorWithId :: ActorId -> [Actor] -> Actor
actorWithId aId actors = actor where
    Just actor = find (\a -> actorId a == aId) actors

--Number of game tiles moved per second
actorVelocity :: Float
actorVelocity = 4

isTraversible :: LevelItem -> Bool
isTraversible (Wall _) = False
isTraversible (GHWall _) = False
isTraversible GHGate = False
isTraversible _ = True

data LevelDataToken = PacmanToken | GhostToken

tokenLevelDataChar :: LevelDataToken -> Char
tokenLevelDataChar PacmanToken = 'p'
tokenLevelDataChar GhostToken = 'g'

idToLevelDataToken :: ActorId -> LevelDataToken
idToLevelDataToken Pacman = PacmanToken
idToLevelDataToken (Ghost _) = GhostToken

initialActor :: ActorId -> [String] -> Actor
initialActor aId levelData = Actor {
    actorId = aId,
    actorSrc = tokenSrc,
    actorDst = tokenDst,
    actorMoveParam = 0.5,
    actorTargetDstFn = targetDstFn
} where
    targetDstFn | aId == Pacman = pacmanTargetDstFn
                | otherwise = ghostTargetDstFn
    char = tokenLevelDataChar . idToLevelDataToken $ aId
    Just rowIndex = findIndex (\row -> char `elem` row) levelData
    cols = elemIndices char (levelData !! rowIndex)
    tokenSrc = (cols !! 0, rowIndex)
    tokenDst = (cols !! 1, rowIndex)

initialActors :: [String] -> [Actor]
initialActors levelData = [
    initialActor Pacman levelData,
    initialActor (Ghost GhostA) levelData,
    initialActor (Ghost GhostB) levelData,
    initialActor (Ghost GhostC) levelData,
    initialActor (Ghost GhostD) levelData
    ]

pacmanTargetDstFn :: Coords -> [Actor] -> Level -> Actor -> Coords
pacmanTargetDstFn inputPacDir actors level a = translateCoords (actorDst a) inputPacDir

ghostTargetDstFn :: Coords -> [Actor] -> Level -> Actor -> Coords
ghostTargetDstFn = pacmanTargetDstFn

--sameDirecTargetDstFn :: Level -> Actor -> Maybe Coords
--sameDirecTargetDstFn level a | isTraversible targetItem = Just targetDst
--                             | otherwise = Nothing
--                             where
--                                direcVec = translateCoords (actorDst a) (negateCoords . actorSrc $ a)
--                                targetDst = translateCoords (actorDst a) direcVec
--                                Just targetItem = M.lookup targetDst (levelItems level)
