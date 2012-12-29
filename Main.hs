module Main(main) where

import Data.IORef
import Graphics.UI.GLUT

import Pacman.World
import Pacman.Level
import qualified Pacman.Engine as Engine

import qualified Pacman.Graphics.Main as Graphics
import qualified Pacman.Graphics.Level as LevelGraphics

import Pacman.InputCommand

main :: IO ()
main = do
    levelData <- readLevelData "01"
    let world = initializeWorld levelData
        input = Nothing :: Maybe InputCommand

        w = floor LevelGraphics.levelItemSize * (levelWidth . worldLevel $ world)
        h = floor LevelGraphics.levelItemSize * (levelHeight . worldLevel $ world)

    createGameWindow w h
    Graphics.setDrawingOptions

    worldRef <- newIORef world
    inputRef <- newIORef input

    displayCallback $= Graphics.render worldRef
    idleCallback $= Just (update worldRef inputRef)
    keyboardMouseCallback $= Just (getInput inputRef)

    mainLoop

readLevelData :: String -> IO [String]
readLevelData levelName = do
        contents <- readFile ("data/levels/" ++ levelName ++ ".txt")
        return (reverse . lines $ contents)

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

update :: IORef World -> IORef (Maybe InputCommand) -> IO ()
update worldRef inputRef = do
    world <- readIORef worldRef
    input <- readIORef inputRef

    newTime <- get elapsedTime
    let dt = fromIntegral newTime / 1000.0 - worldElapsedTime world

    modifyIORef worldRef (\_ -> Engine.updateWorld dt input world)

    postRedisplay Nothing

getInput :: IORef (Maybe InputCommand)-> Key -> KeyState -> t -> t1 -> IO ()
getInput inputRef key state _ _ = do
    prevCommand <- readIORef inputRef 
    let 
        inputCommand (SpecialKey KeyUp) Down = Just MovePacmanUp
        inputCommand (SpecialKey KeyDown) Down = Just MovePacmanDown
        inputCommand (SpecialKey KeyLeft) Down = Just MovePacmanLeft
        inputCommand (SpecialKey KeyRight) Down = Just MovePacmanRight
        inputCommand _ _ = prevCommand

    modifyIORef inputRef (\_ -> inputCommand key state)
