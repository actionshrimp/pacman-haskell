module Pacman.Actors.Scene (Scene(..)) where

import Pacman.Actors.Pacman
import Pacman.Actors.Ghost

data Scene = Scene { width :: Int,
                     height :: Int,
                     pacman :: Pacman,
                     ghosts :: [Ghost]
                   }
