module Pacman.Actors.Scene (Scene(..), initialScene, update) where

import Pacman.Actors.Base

import qualified Pacman.Actors.Pacman as Pacman
import qualified Pacman.Actors.Ghost as Ghost

data Scene = Scene { elapsedTime :: Float,
                     width :: Int,
                     height :: Int,
                     pacman :: Pacman.Pacman,
                     ghosts :: [Ghost.Ghost]
                   }

update :: Scene -> Float -> Scene
update scene dt = Scene {
                        elapsedTime = elapsedTime scene + dt,
                        width = width scene,
                        height = height scene,
                        pacman = Pacman.update (pacman scene) dt,
                        ghosts = map (\x -> Ghost.update x dt) (ghosts scene)
                     }

initialScene = Scene {
    elapsedTime = 0,
    width = 800, 
    height = 600,
    pacman = Pacman.Pacman {
        Pacman.position = (300, 300),
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
        Pacman.queuedDirection = Nothing
    },
    ghosts = [
        Ghost.Ghost {
            Ghost.position = (400, 400),
            Ghost.direction = DRight
        },
        Ghost.Ghost {
            Ghost.position = (500, 400),
            Ghost.direction = DRight
        },
        Ghost.Ghost {
            Ghost.position = (400, 300),
            Ghost.direction = DRight
        },
        Ghost.Ghost {
            Ghost.position = (100, 200),
            Ghost.direction = DRight
        }
    ]}

