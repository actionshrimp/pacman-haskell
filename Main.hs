module Main(main) where

import Data.IORef
import Graphics.UI.GLUT

import Pacman.Actors.Base
import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman

import Pacman.Graphics

scene = Scene.Scene {
    Scene.width = 800, 
    Scene.height = 600,
    Scene.pacman = Pacman.Pacman {
        Pacman.position = (300, 300),
        Pacman.mouthAngle = 0,
        Pacman.mouthAction = Pacman.Opening,
        Pacman.direction = DRight,
        Pacman.queuedDirection = Nothing
    },
    Scene.ghosts = []}

main :: IO ()
main = do
    sceneRef <- newIORef scene
    initializeUI sceneRef
    mainLoop

initializeUI :: IORef Scene.Scene -> IO ()
initializeUI sceneRef = do
    scene <- readIORef sceneRef
    setWindowOptions (Scene.width scene) (Scene.height scene)
    (progname, _) <- getArgsAndInitialize
    createWindow progname
    displayCallback $= (render sceneRef)
    timeRef <- newIORef 0
    idleCallback $= Just (update timeRef sceneRef)

setWindowOptions :: Int -> Int -> IO ()
setWindowOptions width height = do
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    initialDisplayMode $= [DoubleBuffered, WithSamplesPerPixel 4] --AA

render :: IORef Scene.Scene -> IO ()
render sceneRef = do
    renderScene sceneRef
    swapBuffers

update :: IORef Int -> IORef Scene.Scene -> IO ()
update timeRef sceneRef = do
    prevT <- readIORef timeRef
    newT <- get elapsedTime
    let dt = (fromIntegral (newT - prevT)) / 1000.0
    scene <- readIORef sceneRef
    modifyIORef sceneRef (\_ -> Scene.update scene dt)
    modifyIORef timeRef (\_ -> newT)
    postRedisplay Nothing
