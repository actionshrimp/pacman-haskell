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
    width = levelItemSize * (fromIntegral . levelW $ levelData),
    height = levelItemSize * (fromIntegral . levelH $ levelData),
    level = loadLevel levelData,
    pacman = Pacman.Pacman {
        Pacman.position = initialActorPosition levelData 'p',
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
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

initialActorPosition :: [String] -> Char -> Vec2
initialActorPosition levelData actorChar = (levelItemSize * avgX + levelItemSize / 2, levelItemSize * avgY + levelItemSize / 2) where
    avgX = fromIntegral (sum . map fst $ matchingCoords) / fromIntegral (length matchingCoords)
    avgY = fromIntegral (sum . map snd $ matchingCoords) / fromIntegral (length matchingCoords)
    matchingCoords = filter ((== actorChar) . (uncurry (levelItem levelData ' '))) allCoords
    allCoords = [(u, v) | u <- [0..lvlW], v <- [0..lvlH]]
    lvlW = levelW levelData
    lvlH = levelH levelData
