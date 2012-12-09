module Pacman.Actors.Types.Ghost (Ghost(..)) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction

data Ghost = Ghost { position :: Vec2,
                     direction :: Direction,
                     wobbleParam :: Float,
                     eyePosition :: Vec2
                     }
