module Pacman.Graphics.Level (renderLevel) where

import Graphics.Rendering.OpenGL

import Pacman.Graphics.Base

import Pacman.Actors.Types.Level as Level

blue :: Color3 GLfloat
blue = Color3 0 0 1 :: Color3 GLfloat

renderLevel :: Level.Level -> IO ()
renderLevel lvl = do
    let xs = map (*levelItemSize) [0.. length (head lvl)]
        ys = map (*levelItemSize) [0.. length lvl]
    
        coordsGrid = map (zip xs . repeat) ys

        pairedGrid = zipWith (zipWith (\x y -> (x, y))) lvl coordsGrid

    mapM_ (mapM_ renderWall) pairedGrid 

renderWall :: (LevelItem, (Int, Int)) -> IO ()
renderWall (Wall _, (iX, iY)) = do
    let
        x = fromIntegral iX
        y = fromIntegral iY
        w = fromIntegral levelItemSize

        wallPoints = [(x, y), (x + w, y), (x, y + w), (x + w, y + w)]
        wallVerts = map pointToVertex wallPoints

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex wallVerts

renderWall (_, _) = return ()
