module Main(main) where

import Data.IORef

import Pacman.Actors.Base
import Pacman.Actors.Scene
import Pacman.Actors.Pacman

import Pacman.Graphics

scene = Scene {
    width = 800, 
    height = 600,
    pacman = Pacman {
        position = (300, 300),
        mouthAngle = pi / 8,
        direction = DRight,
        queuedDirection = Nothing
    },
    ghosts = []}

main :: IO ()
main = do
    sceneRef <- newIORef scene
    initializeGraphics sceneRef
    renderLoop
