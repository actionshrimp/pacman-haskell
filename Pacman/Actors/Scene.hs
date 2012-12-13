module Pacman.Actors.Scene (Scene(..), loadScene, update) where

import Data.List as List

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
        Pacman.position = actorPosition levelData 'p',
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
        Pacman.queuedDirection = Nothing
    },
    ghosts = [
        Ghost.Ghost {
            Ghost.position = actorPosition levelData 'a',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = actorPosition levelData 'b',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = actorPosition levelData 'c',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        },
        Ghost.Ghost {
            Ghost.position = actorPosition levelData 'd',
            Ghost.direction = DRight,
            Ghost.wobbleParam = 0,
            Ghost.eyePosition = Vec2 {x = 0, y = 0}
        }
    ]}

actorPosition :: [String] -> Char -> Vec2
actorPosition levelData actorChar = Vec2 {
        x = levelItemSize * avgX + levelItemSize / 2,
        y = levelItemSize * avgY + levelItemSize / 2
    } where
    avgX = fromIntegral (sum . map fst $ matchingCoords) / fromIntegral (length matchingCoords)
    avgY = fromIntegral (sum . map snd $ matchingCoords) / fromIntegral (length matchingCoords)
    matchingCoords = filter ((== actorChar) . (uncurry (levelItem levelData ' '))) allCoords
    allCoords = [(u, v) | u <- [0..lvlW], v <- [0..lvlH]]
    lvlW = levelW levelData
    lvlH = levelH levelData
