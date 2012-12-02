module Main(main) where

import Data.IORef

import Pacman.Graphics
import Pacman.Actors

scene = Scene {
    width = 800, 
    height = 600,
    pacman = Pacman {
        position = (300, 300),
        mouthAngle = pi / 8,
        direction = Right,
        queuedDirection = Nothing
    },
    ghosts = []

main :: IO ()
main = do
    sceneRef <- newIORef scene
    initializeGraphics sceneRef
    renderLoop
