module Main(main) where

import Data.IORef
import Graphics.UI.GLUT

import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Graphics.Main as Graphics

import qualified Pacman.Actors.Level as Level

import Pacman.Util.Types.Direction
import qualified Pacman.Util.Types.InputCommands as Cmd

main :: IO ()
main = do
    sceneData <- Level.readLevelData "01"
    let scene = Scene.loadScene sceneData
        input = Nothing :: Maybe Cmd.InputCommand

    createGameWindow (floor . Scene.width $ scene) (floor . Scene.height $ scene)
    Graphics.setDrawingOptions

    sceneRef <- newIORef scene
    inputRef <- newIORef input

    displayCallback $= Graphics.render sceneRef
    idleCallback $= Just (update sceneRef inputRef)
    keyboardMouseCallback $= Just (getInput inputRef)

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

update :: IORef Scene.Scene -> IORef (Maybe Cmd.InputCommand) -> IO ()
update sceneRef inputRef = do
    scene <- readIORef sceneRef
    input <- readIORef inputRef

    newTime <- get elapsedTime
    let dt = fromIntegral newTime / 1000.0 - Scene.elapsedTime scene

    modifyIORef sceneRef (\_ -> Scene.update scene dt input)

    postRedisplay Nothing

getInput :: IORef (Maybe (Cmd.InputCommandT Direction))-> Key -> KeyState -> t -> t1 -> IO ()
getInput inputRef key state _ _ = do
    let 
        inputCommand (SpecialKey KeyUp) Down = Just (Cmd.MovePacman DUp)
        inputCommand (SpecialKey KeyDown) Down = Just (Cmd.MovePacman DDown)
        inputCommand (SpecialKey KeyLeft) Down = Just (Cmd.MovePacman DLeft)
        inputCommand (SpecialKey KeyRight) Down = Just (Cmd.MovePacman DRight)
        inputCommand _ _ = Nothing

    modifyIORef inputRef (\_ -> inputCommand key state)
