module Main(main) where

import Data.IORef
import Graphics.UI.GLUT

import Pacman.Actors.Base
import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman

import qualified Pacman.Graphics as Graphics

main :: IO ()
main = do
    createGameWindow (Scene.width Scene.initialScene) (Scene.height Scene.initialScene)

    sceneRef <- newIORef Scene.initialScene
    displayCallback $= (Graphics.render sceneRef)

    timeRef <- newIORef 0
    idleCallback $= Just (update timeRef sceneRef)

    mainLoop

createGameWindow :: Int -> Int -> IO ()
createGameWindow windowWidth windowHeight = do
    setWindowOptions windowWidth windowHeight
    (progname, _) <- getArgsAndInitialize
    createWindow progname
    return ()

setWindowOptions :: Int -> Int -> IO ()
setWindowOptions width height = do
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    Graphics.setOptions

update :: IORef Int -> IORef Scene.Scene -> IO ()
update timeRef sceneRef = do

    prevT <- readIORef timeRef
    newT <- get elapsedTime
    modifyIORef timeRef (\_ -> newT)

    let dt = (fromIntegral (newT - prevT)) / 1000.0 where

    scene <- readIORef sceneRef
    modifyIORef sceneRef (\_ -> Scene.update scene dt)

    postRedisplay Nothing
