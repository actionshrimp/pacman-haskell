module Pacman.Actors.Types.Scene (Scene(..)) where

import Pacman.Actors.Types.Pacman
import Pacman.Actors.Types.Ghost
import Pacman.Actors.Types.Level

data Scene = Scene { elapsedTime :: Float,
                     width :: Float,
                     height :: Float,
                     pacman :: Pacman,
                     ghosts :: [Ghost],
                     level :: Level
                   }
