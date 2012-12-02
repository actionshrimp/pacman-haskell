module Pacman.Graphics (renderScene) where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix (ortho2D)

import Data.IORef
import qualified Data.Time.Clock as Clock

import qualified Pacman.Actors.Scene as Scene
import qualified Pacman.Actors.Pacman as Pacman

import Pacman.Graphics.Actors

renderScene :: IORef Scene.Scene -> IO ()
renderScene sceneRef = do
    scene <- readIORef sceneRef
    clear [ColorBuffer]

    setProjection (Scene.width scene) (Scene.height scene)
    startDrawMode

    let pacman = Scene.pacman scene
    renderPacman (Pacman.position pacman) (Pacman.mouthAngle pacman) 

setProjection :: Int -> Int -> IO ()
setProjection width height = do
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral width) 0 (fromIntegral height)

startDrawMode :: IO ()
startDrawMode = do
    matrixMode $= (Modelview 0)
    loadIdentity
