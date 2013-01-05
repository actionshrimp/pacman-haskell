module Pacman.Actor (ActorId, ActorIdT(..), GhostId(..), Actor(..), initialActors, actorWithId, actorVelocity, actorTargetDst) where

import Data.List

import Pacman.Util.Coords

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
    actorMoveParam :: Float
}

actorWithId :: ActorId -> [Actor] -> Actor
actorWithId aId actors = actor where
    Just actor = find (\a -> actorId a == aId) actors

--Number of game tiles moved per second
actorVelocity :: Float
actorVelocity = 4

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
    actorMoveParam = 0.5
} where
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

actorTargetDst :: Coords -> [Actor] -> Actor -> Coords
actorTargetDst inputPacDir _ (Actor Pacman _ dst _) = translateCoords dst inputPacDir
actorTargetDst _ actors (Actor (Ghost _) _ _ _) = actorDst (actorWithId Pacman actors)
