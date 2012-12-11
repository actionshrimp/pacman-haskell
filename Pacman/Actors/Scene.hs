module Pacman.Actors.Scene (Scene(..), loadScene, update) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction

import Pacman.Actors.Types.Scene
import qualified Pacman.Actors.Types.Pacman as Pacman
import qualified Pacman.Actors.Types.Ghost as Ghost

import Pacman.Actors.Level
import qualified Pacman.Actors.Pacman as P
import qualified Pacman.Actors.Ghost as G

update :: Scene -> Float -> Scene
update scene dt = Scene {
                        elapsedTime = elapsedTime scene + dt,
                        level = level scene,
                        width = width scene,
                        height = height scene,
                        pacman = P.update (pacman scene) dt,
                        ghosts = map (\x -> G.update x scene dt) (ghosts scene)
                     }


loadScene :: [String] -> Scene
loadScene levelData = Scene {
    elapsedTime = 0,
    width = levelItemSize * length (head levelData),
    height = levelItemSize * length levelData,
    level = loadLevel levelData,
    pacman = Pacman.Pacman {
        Pacman.position = Vec2 {x = 300, y = 300},
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
        Pacman.queuedDirection = Nothing
    },
    ghosts = [
        Ghost.Ghost {
            Ghost.position = Vec2 {x = 400, y = 400},
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = Vec2 {x = 500, y = 400},
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = Vec2 {x = 400, y = 300},
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = Vec2 {x = 100, y = 200},
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        }
    ]}
