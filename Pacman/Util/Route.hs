module Pacman.Util.Route (calculateActorRoute) where

import qualified Debug.Trace as T

import Data.List
import Data.Function
import Data.Maybe

import Pacman.Util.Coords

import Pacman.Level
import Pacman.Actor

data RouteNode = RouteNode { 
    coords :: Coords,
    parent :: Maybe RouteNode,
    score :: Int
} deriving (Eq, Show)

calculateActorRoute :: Level -> Actor -> Coords -> Maybe [Coords]
calculateActorRoute level actor targetCoords = fullRouteTail where
    fullRouteTail = fmap tail fullRoute
    fullRoute = fmap buildRouteFromNode calculatedNode
    calculatedNode = calculateRoute nApplied targetCoords initialOpenList [initialNode]
    nApplied = neighbourNodes level actor targetCoords
    initialOpenList = nApplied initialNode
    initialNode = RouteNode {
            coords = actorDst actor,
            parent = Nothing,
            score = 0 }

--Calculate the route to the target square if one exists
calculateRoute :: (RouteNode -> [RouteNode]) -> Coords -> [RouteNode] -> [RouteNode] -> Maybe RouteNode
calculateRoute neighbourFn targetCoords openList closedList 
                | targetCoordsInClosedList = find isTargetNode closedList
                | null openList = Nothing
                | otherwise = calculateRoute neighbourFn targetCoords newOpenList newClosedList where
                    isTargetNode n = coords n == targetCoords
                    targetCoordsInClosedList = any isTargetNode closedList
                    newOpenList = combineOpenNodes remainingOpenNodes newOpenNodes
                    currentNode : remainingOpenNodes = sortBy (compare `on` score) openList
                    newOpenNodes = filter (\o -> not (any (\c -> coords c == coords o) closedList)) (neighbourFn currentNode)
                    newClosedList = currentNode : closedList
                    combineOpenNodes openA openB = updatedOpenA ++ bsNotInA where
                        (bsAlreadyInA, bsNotInA) = partition (`elem` openA) openB
                        updatedOpenA = map (lowestNode bsAlreadyInA) openA
                        lowestNode bs a = if score a < score b then a else b where
                            b = fromMaybe a (find (\n -> coords n == coords a) bs)

--Build the full route, including the starting node
buildRouteFromNode :: RouteNode -> [Coords]
buildRouteFromNode node = forParent (parent node) ++ [coords node] where
    forParent Nothing = []
    forParent (Just n) = buildRouteFromNode n

neighbourNodes :: Level -> Actor -> Coords -> RouteNode -> [RouteNode]
neighbourNodes level actor targetCoords curNode = 
    allowedNeighbour (0, 1)
    ++ allowedNeighbour (0, -1)
    ++ allowedNeighbour (-1, 0)
    ++ allowedNeighbour (1, 0) where
        allowedNeighbour direc | canWalk level actor attemptCoords = [attemptNode]
                               | otherwise = [] where
                                    curCoords = coords curNode
                                    (unwrappedAttemptX, attemptY) = translateCoords curCoords direc
                                    attemptX | unwrappedAttemptX >= levelWidth level = unwrappedAttemptX - levelWidth level
                                             | unwrappedAttemptX < 0 = unwrappedAttemptX + levelWidth level
                                             | otherwise = unwrappedAttemptX
                                    attemptCoords = (attemptX, attemptY)
                                    attemptScore = score curNode + straightLineScore attemptCoords targetCoords
                                    attemptNode = RouteNode {
                                        coords = attemptCoords,
                                        parent = Just curNode,
                                        score = attemptScore
                                    }

--Whether a level item is traversable, but also prevent ghosts from flipping direction
canWalk :: Level -> Actor -> Coords -> Bool
canWalk level actor c | actorId actor /= Pacman && c == actorSrc actor = False
                      | otherwise = isTraversable level c

straightLineScore :: Coords -> Coords -> Int
straightLineScore start end = (abs . fst $ straightLineCoords start end) + (abs . snd $ straightLineCoords start end) where
    straightLineCoords a b = translateCoords b (negateCoords a)
