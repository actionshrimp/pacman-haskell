module Pacman.Actors.Base (
    canMoveActor,
    moveActor,
    actorsAreTouching) where

import Pacman.Util.Types.Direction
import Pacman.Util.Types.Vec2

import Pacman.Actors.Types.Level

import Pacman.Actors.Level

--Check the front left and front right corners of the actor are on a traversable square
--with a 10% tolerance. MoveActor should lock the actual position into the center again
canMoveActor :: Level -> Direction -> Float -> Vec2 -> Bool 
canMoveActor lvl direc dt (x0, y0) = 
        isTraversable (vecToItem lvl (frontLeftX, frontLeftY)) &&
        isTraversable (vecToItem lvl (frontRightX, frontRightY))
    where
        (frontLeftX, frontLeftY) = (frontX + cw_x * l / 2 * 0.9, frontY + cw_y * l / 2 * 0.9)
        (frontRightX, frontRightY) = (frontX + ccw_x * l / 2 * 0.9, frontY + ccw_y * l / 2 * 0.9)
        (frontX, frontY) = (trgtX + v_x * l / 2, trgtY + v_y * l / 2)
        (trgtX, trgtY) = (x0 + v_x * dt * actorV, y0 + v_y * dt * actorV)
        (cw_x, cw_y) = cw (directionVec direc)
        (ccw_x, ccw_y) = ccw (directionVec direc)
        (v_x, v_y) = directionVec direc
        l = levelItemSize

cw :: Vec2 -> Vec2
cw (x, y) = (-y, x)

ccw :: Vec2 -> Vec2
ccw (x, y) = (y, -x)

moveActor :: Level -> Direction -> Float -> Vec2 -> Vec2 
moveActor lvl direc dt src@(x0, y0) 
    | canMoveActor lvl direc dt src  = (trgtX, trgtY)
    | otherwise = (midX, midY)
    where
        midX = (fromIntegral . floor $ (x0 / l)) * l + l / 2
        midY = (fromIntegral . floor $ (y0 / l)) * l + l / 2
        (trgtX, trgtY) = (wrapX (x0 + v_x * dt * actorV), y0 + v_y * dt * actorV)
        (v_x, v_y) = directionVec direc
        wrapX x | (direc == DLeft) && (x < negate l) = w + l
                | (direc == DRight) && x > (w + l) = negate l
                | otherwise = x
        l = levelItemSize
        w = (fromIntegral . levelW $ lvl) * l

actorsAreTouching :: Vec2 -> Vec2 -> Bool
actorsAreTouching (x1, y1) (x2, y2) = abs (x1 - x2) < levelItemSize && abs (y1 - y2) < levelItemSize
