module Pacman.Graphics.Level (renderLevel) where

import Control.Monad

import Graphics.Rendering.OpenGL hiding (R)

import Pacman.Graphics.Base

import Pacman.Actors.Types.Level as Level

blue :: Color3 GLfloat
blue = Color3 0 0 1 :: Color3 GLfloat

renderLevel :: Level.Level -> IO ()
renderLevel lvl = do
    let xs = map (*levelItemSize) [0.. length (head lvl)]
        ys = map (*levelItemSize) [0.. length lvl]
    
        gridCoords = map (zip xs . repeat) ys

    zipWithM_ (zipWithM_ renderWall) lvl gridCoords

w :: Float
w = fromIntegral levelItemSize

wallPointSets :: WallDirection -> [[(Float, Float)]]
wallPointSets U    = [[(0, 0), (0, 4), (8, 0), (8, 4)]]
wallPointSets D    = [[(0, 4), (0, 8), (8, 4), (8, 8)]]
wallPointSets L    = [[(4, 0), (4, 8), (8, 0), (8, 8)]]
wallPointSets R    = [[(0, 0), (0, 8), (4, 0), (4, 8)]]
wallPointSets CcUL = [[(0, 0), (0, 4), (2, 0), (2, 5), (3, 0), (3, 6), (4, 0), (4, 8)], [(4, 0), (4, 8), (8, 0), (8, 8)]]
wallPointSets _    = [[(0, 0), (0, 8), (8, 0), (8, 8)]]

wallDirColor :: WallDirection -> Color3 GLfloat
--wallDirColor CcUL = Color3 1 0 0 :: Color3 GLfloat
wallDirColor _ = blue

positioned :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
positioned (x, y) = map (\(x1, y1) -> (x + w * x1 / 8, y + w * y1 / 8)) 

renderWall :: LevelItem  -> (Int, Int) -> IO ()
renderWall (Wall direction) (iX, iY) = do
    let
        (x, y) = (fromIntegral iX, fromIntegral iY)
        pointSets = wallPointSets direction
        positionedPointSets = map (positioned (x, y)) pointSets

        wallVerts = map (map pointToVertex) positionedPointSets

    color $ wallDirColor direction
    renderPrimitive TriangleStrip $
        mapM_ (mapM_ vertex) wallVerts

renderWall _ _ = return ()
