module Pacman.Actors.Types.Pacman (MouthAction(..), Pacman(..)) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction

data MouthAction = Opening | Closing
data Pacman = Pacman { position :: Vec2,
                       mouthAngle :: Float,
                       mouthAction :: MouthAction,
                       direction :: Direction,
                       queuedDirection :: Maybe Direction }

