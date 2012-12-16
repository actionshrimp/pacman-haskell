module Pacman.Graphics.Main (render, setWindowOptions, setDrawingOptions) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import qualified Pacman.Actors.Scene as Scene

import Pacman.Graphics.Pacman
import Pacman.Graphics.Ghost
import Pacman.Graphics.Level

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

render :: IORef Scene.Scene -> IO ()
render sceneRef = do
    scene <- readIORef sceneRef

    clear [ColorBuffer]

    setProjection (Scene.width scene) (Scene.height scene)
    startDrawMode

    renderScene scene

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

renderScene :: Scene.Scene -> IO ()
renderScene scene = do
    renderLevel (Scene.level scene) (Scene.elapsedTime scene)
    renderPacman (Scene.pacman scene)
    mapM_ (\(i, ghost) -> renderGhost ghost i) (zip [1..] (Scene.ghosts scene))
