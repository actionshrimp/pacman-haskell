module Pacman.Actors.Ghost (Ghost) where

import Pacman.Actors.Base

data Ghost = Ghost { position :: Vec2,
                     direction :: Direction }
