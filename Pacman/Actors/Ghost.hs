module Pacman.Actors.Ghost (Ghost(..), update) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction
import Pacman.Util.Functions

import Pacman.Actors.Types.Ghost
import Pacman.Actors.Types.Scene

wobbleVelocity ::  Float
wobbleVelocity = 0.5

update :: Ghost -> Scene -> Float -> Ghost
update ghost scene dt = Ghost { position = position ghost,
                                direction = updateDirection (direction ghost) (elapsedTime scene),
                                wobbleParam = cyclingParam (wobbleParam ghost + wobbleVelocity * dt) 0 1,
                                eyePosition = updateEyePosition (eyePosition ghost) (direction ghost) dt
                                }

baseEyeVelocity ::  Float
baseEyeVelocity = 3.0

updateEyePosition :: Vec2 -> Direction -> Float -> Vec2
updateEyePosition Vec2 {x = curX, y = curY} direc dt = Vec2 {x = curX + vX * dt, y = curY + vY * dt} where vX = baseEyeVelocity * (targetX - curX)
                                                                                                           vY = baseEyeVelocity * (targetY - curY)
                                                                                                           Vec2 {x = targetX, y = targetY} = directionVec direc
updateDirection :: Direction -> Float -> Direction
updateDirection curDirec totalTime | timeMod20 > 15 = DRight
                                   | timeMod20 > 10 = DUp
                                   | timeMod20 > 5 = DLeft
                                   | otherwise = DDown
                                   where timeMod20 = ((truncate totalTime) `mod` 20)
