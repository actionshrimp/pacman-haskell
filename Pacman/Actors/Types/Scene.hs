module Pacman.Actors.Types.Scene (Scene(..)) where

import Pacman.Actors.Types.Pacman
import Pacman.Actors.Types.Ghost
import Pacman.Actors.Types.Level

data Scene = Scene { elapsedTime :: Float,
                     width :: Int,
                     height :: Int,
                     pacman :: Pacman,
                     ghosts :: [Ghost],
                     level :: Level
                   }
