module Pacman.Actors.Ghost (Ghost(..), update) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction
import Pacman.Util.Functions

import Pacman.Actors.Types.Ghost
import Pacman.Actors.Types.Scene

import Pacman.Actors.Level

wobbleVelocity ::  Float
wobbleVelocity = 0.5

update :: Scene -> Float -> Ghost -> Ghost
update scene dt ghost = Ghost { position = moveActor (level scene) newDirection dt (position ghost),
                                direction = newDirection,
                                wobbleParam = cyclingParam (wobbleParam ghost + wobbleVelocity * dt) 0 1,
                                eyePosition = updateEyePosition (eyePosition ghost) newDirection dt
                                } where
                                    newDirection = updateDirection (direction ghost) (elapsedTime scene)

baseEyeVelocity ::  Float
baseEyeVelocity = 3.0

updateEyePosition :: Vec2 -> Direction -> Float -> Vec2
updateEyePosition (curX, curY) direc dt = (curX + vX * dt, curY + vY * dt) where 
                                            vX = baseEyeVelocity * (targetX - curX)
                                            vY = baseEyeVelocity * (targetY - curY)
                                            (targetX,  targetY) = directionVec direc

updateDirection :: Direction -> Float -> Direction
updateDirection curDirec totalTime | timeMod20 >= 15 = DRight
                                   | timeMod20 >= 10 = DUp
                                   | timeMod20 >= 5 = DLeft
                                   | otherwise = DDown
                                   where timeMod20 = (truncate totalTime) `mod` 20
