module Pacman.Graphics.Main (render, setWindowOptions, setDrawingOptions) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Pacman.World
import Pacman.Level

import Pacman.Graphics.Level
import Pacman.Graphics.Pacman
import Pacman.Graphics.Ghost

setWindowOptions :: IO()
setWindowOptions =
    initialDisplayMode $= [DoubleBuffered, WithSamplesPerPixel 12] --AA

setDrawingOptions :: IO ()
setDrawingOptions =
    depthFunc $= Nothing
    --blend $= Enabled
    --blendEquation $= FuncAdd
    --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    --lineSmooth $= Enabled

render :: IORef World -> IO ()
render worldRef = do
    world <- readIORef worldRef

    clear [ColorBuffer]

    let w = (*levelItemSize) . fromIntegral . levelWidth . worldLevel $ world
        h = (*levelItemSize) . fromIntegral . levelHeight . worldLevel $ world


    setProjection w h
    startDrawMode

    renderWorld world

    swapBuffers

setProjection :: Float -> Float -> IO ()
setProjection width height = do
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (realToFrac width) 0 (realToFrac height)

startDrawMode :: IO ()
startDrawMode = do
    matrixMode $= Modelview 0
    loadIdentity

renderWorld :: World -> IO ()
renderWorld world = do
    renderLevel world
    renderPacman world
    renderGhosts world
