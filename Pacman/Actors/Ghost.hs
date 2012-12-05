module Pacman.Actors.Ghost (Ghost(..), update) where

import Pacman.Actors.Base

data Ghost = Ghost { position :: Vec2,
                     direction :: Direction,
                     wobbleParam :: Float
                     }

wobbleVelocity ::  Float
wobbleVelocity = 0.5

update :: Ghost -> Float -> Ghost
update ghost dt = Ghost { position = position ghost,
                          direction = direction ghost,
                          wobbleParam = cyclingParam (wobbleParam ghost + wobbleVelocity * dt) 0 1
                          }
