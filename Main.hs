module Main(main) where

import Data.IORef
import Graphics.UI.GLUT

import qualified Pacman.Actors.Scene as Scene

import qualified Pacman.Graphics as Graphics

main :: IO ()
main = do
    createGameWindow (Scene.width Scene.initialScene) (Scene.height Scene.initialScene)
    Graphics.setDrawingOptions

    sceneRef <- newIORef Scene.initialScene
    displayCallback $= Graphics.render sceneRef
    idleCallback $= Just (update sceneRef)

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
    Graphics.setWindowOptions

update :: IORef Scene.Scene -> IO ()
update sceneRef = do
    scene <- readIORef sceneRef

    newTime <- get elapsedTime
    let dt = (fromIntegral newTime / 1000.0) - (Scene.elapsedTime scene)

    modifyIORef sceneRef (\_ -> Scene.update scene dt)

    postRedisplay Nothing
