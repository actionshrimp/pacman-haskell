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

wallPoints :: WallDirection -> [(Float, Float)]
wallPoints U    = [(0, 0), (0, 16), (32, 16), (32, 0)]
wallPoints D    = [(0, 16), (0, 32), (32, 32), (32, 16)]
wallPoints L    = [(16, 0), (16, 32), (32, 32), (32, 0)]
wallPoints R    = [(0, 0), (0, 32), (16, 32), (16, 0)]
wallPoints CcUL = [(32, 0), (0, 0), (0, 16), (8, 20), (12, 24), (16, 32), (32, 32)]
wallPoints CcUR = [(0, 0), (0, 32), (16, 32), (20, 24), (24, 20), (32, 16), (32, 0)]
wallPoints CcDR = [(0, 32), (32, 32), (32, 16), (24, 12), (20, 8), (16, 0), (0, 0)]
wallPoints CcDL = [(32, 32), (32, 0), (16, 0), (12, 8), (8, 12), (0, 16), (0, 32)]
wallPoints CvUL = [(32, 0), (16, 0), (16, 4), (17, 8), (19, 11), (21, 13), (24, 15), (28, 16), (32, 16)]
wallPoints CvUR = [(0, 0), (0, 16), (4, 16), (8, 15), (11, 13), (13, 11), (15, 8), (16, 4), (16, 0)]
wallPoints CvDR = [(0, 32), (16, 32), (16, 28), (15, 24), (13, 21), (11, 19), (8, 17), (4, 16), (0, 16)]
wallPoints CvDL = [(32, 32), (32, 16), (28, 16), (24, 17), (21, 19), (19, 21), (17, 24), (16, 28), (16, 32)]
wallPoints _    = [(0, 0), (0, 32), (32, 32), (32, 0)]

positioned :: (Float, Float) -> (Float, Float) -> (Float, Float)
positioned (x, y) (x1, y1) =  (x + w * x1 / 32, y + w * y1 / 32)

renderWall :: LevelItem  -> (Int, Int) -> IO ()
renderWall (Wall direction) (iX, iY) = do
    let
        (x, y) = (fromIntegral iX, fromIntegral iY)
        points = wallPoints direction
        positionedPoints = map (positioned (x, y)) points

        verts = map pointToVertex positionedPoints

    color blue
    renderPrimitive TriangleFan $
        mapM_ vertex verts

renderWall _ _ = return ()
