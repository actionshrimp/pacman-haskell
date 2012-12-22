module Pacman.Actors.Pacman (MouthAction(..), Pacman(..), update) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction
import qualified Pacman.Util.Types.InputCommands as Cmd

import Pacman.Actors.Types.Pacman
import Pacman.Actors.Types.Scene
import Pacman.Actors.Types.Level

import Pacman.Actors.Level

update :: Scene -> Float -> Maybe Cmd.InputCommand -> Pacman -> Pacman
update scene dt input pacman = Pacman {
                        position = moveActor (level scene) (direction pacman) dt (position pacman),
                        mouthAngle = newAngle,
                        mouthAction = newAction,
                        direction = updateDirection input (level scene) (position pacman) dt (direction pacman),
                        queuedDirection = queuedDirection pacman
                     } where
                     (newAngle, newAction) = updateMouth 
                        (mouthAngle pacman)
                        (mouthAction pacman)
                        dt

--Angular velocity for the mouth
mouthW :: Float
mouthW = 5

maxMouthAngle :: Float
maxMouthAngle = pi / 4

updateMouth :: Float -> MouthAction -> Float -> (Float, MouthAction)
updateMouth currentAngle Opening dt | (currentAngle + (mouthW * dt)) > maxMouthAngle = 
                                        (maxMouthAngle, Closing)
                                    | otherwise = 
                                        (currentAngle + (mouthW * dt), Opening)

updateMouth currentAngle Closing dt | (currentAngle - (mouthW * dt)) < 0 = 
                                        (0, Opening)
                                    | otherwise = 
                                        (currentAngle - (mouthW * dt), Closing)

updateDirection :: Maybe (Cmd.InputCommandT Direction) -> Level -> Vec2 -> Float -> Direction -> Direction
updateDirection (Just (Cmd.MovePacman newDir)) level pos dt oldDir | canMoveActor level newDir dt pos = newDir
                                                                   | otherwise = oldDir
updateDirection _ _ _ _ curDirection = curDirection
