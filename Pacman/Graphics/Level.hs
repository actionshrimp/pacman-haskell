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
        points = translateItem dest $ wall direction
        verts = map pointToVertex points

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex verts

renderLevelItem _ (GHWall direction) dest = do
    let
        points = translateItem dest $ ghWall direction
        verts = map pointToVertex points

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex verts

renderLevelItem _ GHGate dest = do
    let
        points = translateItem dest ghGate
        verts = map pointToVertex points

    color white
    renderPrimitive TriangleStrip $
        mapM_ vertex verts
    
renderLevelItem _ (Pickup Pill) (x, y) = renderPill 3 (x, y)
renderLevelItem t (Pickup PowerPill) (x, y) = renderPill r_t (x, y) where
    r_t = 7 * (sin (5*t) ** 2) + 2

renderLevelItem _ _ _ = return ()

--(Wall thickness, Wall radius)
wT ::  Float
wR ::  Float
(wT, wR) = (0.25, 0.65)

curvedWall :: (Float, Float) -> Float -> Float -> Float -> [(Float, Float)]
curvedWall gridTileDest r t0 t1 = concat wallCurve where
    wallCurve = map (translateItem gridTileDest) curve
    curve = foldr (\t acc -> [circlePt r t, circlePt (r + wT) t] : acc) [] tPoints
    tPoints = [t0, t0 +  t1/12 .. t1]

wall :: WallDirection -> [(Float, Float)]
wall U    = scaleItem levelItemSize [(0, wR - wT), (0, wR), (1, wR - wT), (1, wR)]
wall D    = scaleItem levelItemSize [(0, 1 - wR), (0, 1 - wR + wT), (1, 1 - wR), (1, 1 - wR + wT)]
wall L    = scaleItem levelItemSize [(1 - wR, 0), (1 - wR, 1), (1 - wR + wT, 0), (1 - wR + wT, 1)]
wall R    = scaleItem levelItemSize [(wR - wT, 0), (wR - wT, 1), (wR, 0), (wR, 1)]

wall CcDL = scaleItem levelItemSize $ curvedWall (0, 0) (1 - wR) 0 (pi / 2)
wall CcDR = scaleItem levelItemSize $ curvedWall (1, 0) (1 - wR) (pi / 2) pi
wall CcUR = scaleItem levelItemSize $ curvedWall (1, 1) (1 - wR) pi (3 * pi / 2)
wall CcUL = scaleItem levelItemSize $ curvedWall (0, 1) (1 - wR) (3 * pi / 2) (2 * pi)

wall CvUL = scaleItem levelItemSize $ curvedWall (1, 0) (wR - wT) (pi / 2) pi
wall CvUR = scaleItem levelItemSize $ curvedWall (0, 0) (wR - wT) 0 (pi / 2)
wall CvDL = scaleItem levelItemSize $ curvedWall (1, 1) (wR - wT) pi (3 * pi / 2)
wall CvDR = scaleItem levelItemSize $ curvedWall (0, 1) (wR - wT) (3 * pi / 2) (2 * pi)
wall _    = []

ghwT :: Float
ghwT = 0.3

ghWall :: GHWallDirection -> [(Float, Float)]
ghWall UL = scaleItem levelItemSize [(0.5 - ghwT / 2, 0), (0.5 + ghwT / 2, 0), (0.5 - ghwT / 2, 0.5 + ghwT / 2), (0.5 + ghwT / 2, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2), (1, 0.5 - ghwT / 2)]
ghWall UR = scaleItem levelItemSize [(0, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0.5 - ghwT / 2), (0.5 + ghwT / 2, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0), (0.5 + ghwT / 2, 0)]
ghWall DL = scaleItem levelItemSize [(1, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0.5 - ghwT / 2), (0.5 + ghwT / 2, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 1)]
ghWall DR = scaleItem levelItemSize [(0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 1), (0.5 - ghwT / 2, 0.5 + ghwT / 2), (0.5 + ghwT / 2, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (0, 0.5 - ghwT / 2)]
ghWall H  = scaleItem levelItemSize [(0, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (1, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2)]
ghWall V  = scaleItem levelItemSize [(0.5 - ghwT / 2, 0), (0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 0), (0.5 + ghwT / 2, 1)]

ghgT :: Float
ghgT = 0.1

ghGate = scaleItem levelItemSize [(0, 0.5 - ghgT / 2), (0, 0.5 + ghgT / 2), (1, 0.5 - ghgT / 2), (1, 0.5 + ghgT / 2)]

renderPill :: Float -> (Float, Float) -> IO ()
renderPill r dest = do
    let
        --Position the pill halfway into the level tile
        m = levelItemSize / 2.0
        points = translateItem (m, m) $ map (circlePt r) [0, pi/8..2*pi]

        --Position the level tile in the level
        positionedPoints = translateItem dest points

        verts = map pointToVertex positionedPoints

    color white
    renderPrimitive TriangleFan $
        mapM_ vertex verts
