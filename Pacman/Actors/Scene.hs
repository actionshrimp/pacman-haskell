module Pacman.Actors.Scene (Scene(..), loadScene, update, moveActor, canMoveActor, actorsAreTouching) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction
import qualified Pacman.Util.Types.InputCommands as Cmd

import Pacman.Actors.Types.Scene
import qualified Pacman.Actors.Types.Pacman as Pacman
import qualified Pacman.Actors.Types.Ghost as Ghost

import Pacman.Actors.Base
import Pacman.Actors.Level
import qualified Pacman.Actors.Pacman as P
import qualified Pacman.Actors.Ghost as G

update :: Scene -> Float -> Maybe Cmd.InputCommand -> Scene
update scene dt input = Scene {
                        elapsedTime = elapsedTime scene + dt,
                        level = level scene,
                        width = width scene,
                        height = height scene,
                        pacman = P.update scene dt input (pacman scene),
                        ghosts = map (G.update scene dt) (ghosts scene)
                     }


loadScene :: [String] -> Scene
loadScene levelData = Scene {
    elapsedTime = 0,
    width = levelItemSize * (fromIntegral . levelW $ levelData),
    height = levelItemSize * (fromIntegral . levelH $ levelData),
    level = loadLevel levelData,
    pacman = Pacman.Pacman {
        Pacman.position = initialActorPosition levelData 'p',
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
        Pacman.prevDirection = DRight,
        Pacman.directionChangeParam = 1,
        Pacman.queuedDirection = Nothing
    },
    ghosts = [
        Ghost.Ghost {
            Ghost.position = initialActorPosition levelData 'a',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = (0, 0)
        },
        Ghost.Ghost {
            Ghost.position = initialActorPosition levelData 'b',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = (0, 0)
        },
        Ghost.Ghost {
            Ghost.position = initialActorPosition levelData 'c',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = (0, 0)
        },
        Ghost.Ghost {
            Ghost.position = initialActorPosition levelData 'd',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = (0, 0)
        }
    ]}
