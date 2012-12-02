module Pacman.Actors.Pacman (Pacman(..), update) where

import Pacman.Actors.Base

data Pacman = Pacman { position :: Vec2,
                       mouthAngle :: Float,
                       direction :: Direction,
                       queuedDirection :: Maybe Direction }

update :: Pacman -> Int -> Pacman
update pacman dt = Pacman {
                        position = position pacman,
                        mouthAngle = (mouthAngle pacman) + 0.001,
                        direction = direction pacman,
                        queuedDirection = queuedDirection pacman
                     }
