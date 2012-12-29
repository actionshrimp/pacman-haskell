module Pacman.Actor (ActorId, ActorIdT(..), GhostId(..), Actor(..), initialActors, actorWithId, actorUpdate) where

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
    actorTargetDstFn :: Maybe InputCommand -> [Actor] -> Level -> Actor -> Maybe Coords
}

actorWithId :: ActorId -> [Actor] -> Actor
actorWithId aId actors = actor where
    Just actor = find (\a -> actorId a == aId) actors

actorParamVelocity = 4

isTraversible :: LevelItem -> Bool
isTraversible (Wall _) = False
isTraversible (GHWall _) = False
isTraversible GHGate = False
isTraversible _ = True

actorUpdate :: Float -> Coords -> Maybe Coords -> Actor -> Actor
actorUpdate dt levelSize maybeTargetDst a = Actor {
    actorId = actorId a,
    actorSrc = newSrcWrapped,
    actorDst = newDstWrapped,
    actorMoveParam = newMoveParam,
    actorTargetDstFn = actorTargetDstFn a
} where
    oldMoveParam = actorMoveParam a
    newMoveParam | (targetDst == oldDst) && (oldMoveParam >= 1) = oldMoveParam
                 | oldMoveParam >= 1 = oldMoveParam - 1
                 | otherwise = oldMoveParam + actorParamVelocity * dt
    oldSrc = actorSrc a
    oldDst = actorDst a
    newSrc | newMoveParam < oldMoveParam = oldDst
           | otherwise = oldSrc
    newDst | newMoveParam < oldMoveParam = targetDst
           | otherwise = oldDst
    direcVec = direcVecCoords newSrc newDst
    newDstWrapped | fst newSrc > fst levelSize && (direcVec == (1, 0)) = (fst newDst - (fst levelSize + 2), snd newDst)
                  | fst newSrc < 0 && (direcVec == (-1, 0)) = (fst newDst + (fst levelSize + 2), snd newDst)
                  | otherwise = newDst
    newSrcWrapped | fst newSrc > fst levelSize && (direcVec == (1, 0)) = (fst newSrc - (fst levelSize + 2), snd newSrc)
                  | fst newSrc < 0 && (direcVec == (-1, 0))  = (fst newSrc + (fst levelSize + 2), snd newDst)
                  | otherwise = newSrc
    targetDst = fromMaybe oldDst maybeTargetDst

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
initialActors levelData = initialActor Pacman levelData : [initialActor (Ghost GhostA) levelData]

pacmanTargetDstFn :: Maybe InputCommand -> [Actor] -> Level -> Actor -> Maybe Coords
pacmanTargetDstFn (Just cmd) _ level a | isTraversible targetItem = Just targetDst
                                            | otherwise = sameDirecTargetDstFn level a
                            where 
                                targetDst = translateCoords (moveCmdVec cmd) (actorDst a)
                                targetItem = fromMaybe Blank (M.lookup targetDst (levelItems level))

pacmanTargetDstFn Nothing _ level a = sameDirecTargetDstFn level a

moveCmdVec :: InputCommand -> Coords
moveCmdVec MovePacmanUp    = (0, 1)
moveCmdVec MovePacmanDown  = (0, -1)
moveCmdVec MovePacmanLeft  = (-1, 0)
moveCmdVec MovePacmanRight = (1, 0)

ghostTargetDstFn :: Maybe InputCommand -> [Actor] -> Level -> Actor -> Maybe Coords
ghostTargetDstFn _ _ level a = sameDirecTargetDstFn level a

sameDirecTargetDstFn :: Level -> Actor -> Maybe Coords
sameDirecTargetDstFn level a | isTraversible targetItem = Just targetDst
                             | otherwise = Nothing
                             where
                                direcVec = translateCoords (actorDst a) (negateCoords . actorSrc $ a)
                                targetDst = translateCoords (actorDst a) direcVec
                                Just targetItem = M.lookup targetDst (levelItems level)
