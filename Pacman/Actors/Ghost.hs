module Pacman.Actors.Ghost (Ghost(..), update) where

import Pacman.Actors.Base

data Ghost = Ghost { position :: Vec2,
                     direction :: Direction
                     }

update :: Ghost -> Float -> Ghost
update ghost dt = ghost
