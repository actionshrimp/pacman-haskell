module Pacman.Graphics.Level (renderLevel) where

import Control.Monad
import Pacman.Util.Functions

import Graphics.Rendering.OpenGL hiding (R)

import Pacman.Graphics.Base

import Pacman.Actors.Types.Level as Level
import Pacman.Actors.Level
red :: Color3 GLfloat
red = Color3 1 0 0 :: Color3 GLfloat

blue :: Color3 GLfloat
blue = Color3 0 0 1 :: Color3 GLfloat

white :: Color3 GLfloat
white = Color3 1 1 1 :: Color3 GLfloat

renderLevel :: Level.Level -> Float -> IO ()
renderLevel lvl t = do
    let xs = map ((*levelItemSize) . fromIntegral) [0.. levelW lvl]
        ys = map ((*levelItemSize) . fromIntegral) [0.. levelH lvl]
    
        gridCoords = map (zip xs . repeat) ys

    zipWithM_ (zipWithM_ (renderLevelItem t)) lvl gridCoords

renderLevelItem :: Float -> LevelItem  -> (Float, Float) -> IO ()
renderLevelItem _ (Wall direction) dest = do
    let
        points = translatePts dest $ wall direction
        verts = map pointToVertex points

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex verts

renderLevelItem _ (Pickup Pill) (x, y) = renderPill 3 (x, y)
renderLevelItem t (Pickup PowerPill) (x, y) = renderPill r_t (x, y) where
    r_t = 7 * (sin (5*t) ** 2) + 2
    
renderLevelItem _ _ _ = return ()

--level tile length
l :: Float
l = levelItemSize

--Wall thickness
wT :: Float
wT = 8

--Wall radius
wR :: Float
wR = 16

ccWall :: (Float, Float) -> Float -> Float -> Float -> [(Float, Float)]
ccWall gridTileDest r t0 t1 = concat wallCurve where
    wallCurve = map (translatePts gridTileDest) curve
    curve = foldr (\t acc -> [circlePt r t, circlePt (r + wT) t] : acc) [] tPoints
    tPoints = [t0, t0 +  t1/12 .. t1]

wall :: WallDirection -> [(Float, Float)]
wall U    = scaleWall [(0, 16 - wT), (0, 16), (32, 16 - wT), (32, 16)]
wall D    = scaleWall [(0, 16), (0, 16 + wT), (32, 16), (32, 16 + wT)]
wall L    = scaleWall [(16, 0), (16, 32), (16 + wT, 0), (16 + wT, 32)]
wall R    = scaleWall [(16 - wT, 0), (16 - wT, 32), (16, 0), (16, 32)]

wall CcDL = scaleWall $ ccWall (0, 0) wR 0 (pi / 2)
wall CcDR = scaleWall $ ccWall (32, 0) wR (pi / 2) pi
wall CcUR = scaleWall $ ccWall (32, 32) wR pi (3 * pi / 2)
wall CcUL = scaleWall $ ccWall (0, 32) wR (3 * pi / 2) (2 * pi)

wall CvUL = scaleWall $ ccWall (32, 0) (wR - wT) (pi / 2) pi
wall CvUR = scaleWall $ ccWall (0, 0) (wR - wT) 0 (pi / 2)
wall CvDL = scaleWall $ ccWall (32, 32) (wR - wT) pi (3 * pi / 2)
wall CvDR = scaleWall $ ccWall (0, 32) (wR - wT) (3 * pi / 2) (2 * pi)
wall _    = scaleWall []

scaleWall :: [(Float, Float)] -> [(Float, Float)]
scaleWall = map scaleWallPoint where
                    scaleWallPoint (x, y) = (l * x / 32, l * y / 32)

renderPill :: Float -> (Float, Float) -> IO ()
renderPill r dest = do
    let
        m = l/2.0
        points = map (\t -> (m + r * cos t, m + r * sin t)) [0, pi/8..2*pi]
        positionedPoints = translatePts dest points

        verts = map pointToVertex positionedPoints

    color white
    renderPrimitive TriangleFan $
        mapM_ vertex verts
