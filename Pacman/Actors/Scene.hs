module Pacman.Actors.Scene (Scene(..), update) where

import qualified Pacman.Actors.Pacman as Pacman
import qualified Pacman.Actors.Ghost as Ghost

data Scene = Scene { width :: Int,
                     height :: Int,
                     pacman :: Pacman.Pacman,
                     ghosts :: [Ghost.Ghost]
                   }

update :: Scene -> Int -> Scene
update scene dt = Scene {
                        width = width scene,
                        height = height scene,
                        pacman = Pacman.update (pacman scene) dt,
                        ghosts = map (\x -> Ghost.update x dt) (ghosts scene)
                     }
