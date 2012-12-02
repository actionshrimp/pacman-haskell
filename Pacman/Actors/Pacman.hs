module Pacman.Actors.Pacman (Pacman(..)) where

import Pacman.Actors.Base

data Pacman = Pacman { position :: Vec2,
                       mouthAngle :: Float,
                       direction :: Direction,
                       queuedDirection :: Maybe Direction }
