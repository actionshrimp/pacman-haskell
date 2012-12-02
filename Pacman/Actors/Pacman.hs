module Pacman.Actors.Pacman (MouthAction(..), Pacman(..), update) where

import Pacman.Actors.Base

data MouthAction = Opening | Closing
data Pacman = Pacman { position :: Vec2,
                       mouthAngle :: Float,
                       mouthAction :: MouthAction,
                       direction :: Direction,
                       queuedDirection :: Maybe Direction }

update :: Pacman -> Float -> Pacman
update pacman dt = Pacman {
                        position = position pacman,
                        mouthAngle = newAngle,
                        mouthAction = newAction,
                        direction = direction pacman,
                        queuedDirection = queuedDirection pacman
                     } where
                     (newAngle, newAction) = updateMouth 
                        (mouthAngle pacman)
                        (mouthAction pacman)
                        dt

--Angular velocity for the mouth
mouthW = 3
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
