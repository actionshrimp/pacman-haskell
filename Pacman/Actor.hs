module Pacman.Actor (Actor(..), ActorIdT(..), GhostId(..), ActorId, actorVelocity, actorWithId, initialActors, ghostTargetDst) where

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

ghostTargetDst :: Bool -> Bool -> Int -> Int -> [Actor] -> Actor -> Coords
ghostTargetDst True _ levelW levelH _ a = scatterTile levelW levelH gId where Ghost gId = actorId a
ghostTargetDst _ _ levelW levelH actors (Actor (Ghost GhostA) _ _ _) = actorDst (actorWithId Pacman actors)
ghostTargetDst _ _ levelW levelH actors (Actor (Ghost GhostB) _ _ _) = translateCoords pacmanDst (5 * fst pacmanDir, 5 * snd pacmanDir) where
    pacman = actorWithId Pacman actors
    pacmanDst = actorDst pacman
    pacmanDir = direcVecCoords (actorSrc pacman) pacmanDst
ghostTargetDst _ _ levelW levelH actors (Actor (Ghost GhostC) _ _ _) = targetSquare where
    targetSquare = translateCoords ghostADst doubleGhostAToInFrontOfPacman
    doubleGhostAToInFrontOfPacman = (2 * fst ghostAToInFrontOfPacman, 2 * snd ghostAToInFrontOfPacman)
    ghostAToInFrontOfPacman = translateCoords inFrontOfPacman (negateCoords ghostADst)
    ghostADst = actorDst ghostA
    inFrontOfPacman = translateCoords pacmanDst (2 * fst pacmanDir, 2 * snd pacmanDir)
    pacmanDst = actorDst pacman
    pacmanDir = direcVecCoords (actorSrc pacman) pacmanDst
    pacman = actorWithId Pacman actors
    ghostA = actorWithId (Ghost GhostA) actors
ghostTargetDst _ _ levelW levelH actors (Actor (Ghost GhostD) _ _ _) | distance > 8 = actorDst (actorWithId Pacman actors)
                                                   | otherwise = scatterTile levelW levelH GhostD where
                                                   distance = sqrt (fromIntegral a ** 2 + fromIntegral b ** 2)
                                                   (a, b) = translateCoords pacmanDst (negateCoords ghostDst)
                                                   pacmanDst = actorDst (actorWithId Pacman actors)
                                                   ghostDst = actorDst (actorWithId (Ghost GhostD) actors)

scatterTile :: Int -> Int -> GhostId -> Coords
scatterTile w h GhostA = (w - 5, h - 2)
scatterTile _ h GhostB = (4, h - 2)
scatterTile w _ GhostC = (w - 5, 1)
scatterTile _ _ GhostD = (4, 1)
