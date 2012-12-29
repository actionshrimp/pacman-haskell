module Pacman.Graphics.Level (renderLevel, levelItemSize) where

import qualified Data.Map as M

import Graphics.Rendering.OpenGL hiding (R, Level)

import Pacman.World
import Pacman.Level
import Pacman.Effects

import Pacman.Graphics.Vertex

red :: Color3 GLfloat
red = Color3 1 0 0 :: Color3 GLfloat

blue :: Color3 GLfloat
blue = Color3 0 0 1 :: Color3 GLfloat

white :: Color3 GLfloat
white = Color3 1 1 1 :: Color3 GLfloat

levelItemSize :: Float
levelItemSize = 26

renderLevel :: World -> IO ()
renderLevel world = do
    let 
        lvl = worldLevel world
        effects = worldEffects world
        scaleForRender (cX, cY) = (fromIntegral cX * levelItemSize, fromIntegral cY * levelItemSize)
        items = M.foldWithKey (\coords item list -> (scaleForRender coords, item) : list) [] (levelItems lvl)
        
        pillPulsateParam = pillPulsateEffectValue . pillPulsateEffect $ effects

    mapM_ (uncurry renderStaticItem) items
    mapM_ (uncurry (renderPulsatingPill pillPulsateParam)) items

renderStaticItem :: (Float, Float) -> LevelItem -> IO ()
renderStaticItem dst (Wall direction) = do
    let
        points = map (translatePoint dst) $ wall direction
        verts = map pointToVertex points

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex verts

renderStaticItem dst (GHWall direction) = do
    let
        points = map (translatePoint dst) $ ghWall direction
        verts = map pointToVertex points

    color blue
    renderPrimitive TriangleStrip $
        mapM_ vertex verts

renderStaticItem dst GHGate = do
    let
        points = map (translatePoint dst) ghGate
        verts = map pointToVertex points

    color white
    renderPrimitive TriangleStrip $
        mapM_ vertex verts
    
renderStaticItem dst (Pickup Pill) = renderPill 3 dst

renderStaticItem _ _ = return ()

renderPulsatingPill :: Float -> (Float, Float) -> LevelItemT PickupType t t1 -> IO ()
renderPulsatingPill t dst (Pickup PowerPill) = renderPill r_t dst where
    r_t = 7 * (t ** 2) + 2

renderPulsatingPill _ _ _ = return ()

--(Wall thickness, Wall radius)
wT ::  Float
wR ::  Float
(wT, wR) = (0.25, 0.65)

curvedWall :: (Float, Float) -> Float -> Float -> Float -> [(Float, Float)]
curvedWall gridTileDest r t0 t1 = wallCurve where
    wallCurve = map (translatePoint gridTileDest) curve
    curve = foldr (\t acc -> [circlePt r t, circlePt (r + wT) t] ++ acc) [] tPoints
    tPoints = [t0, t0 +  t1/12 .. t1]

wall :: WallDirection -> [(Float, Float)]
wall U    = map (scalePoint levelItemSize) [(0, wR - wT), (0, wR), (1, wR - wT), (1, wR)]
wall D    = map (scalePoint levelItemSize) [(0, 1 - wR), (0, 1 - wR + wT), (1, 1 - wR), (1, 1 - wR + wT)]
wall L    = map (scalePoint levelItemSize) [(1 - wR, 0), (1 - wR, 1), (1 - wR + wT, 0), (1 - wR + wT, 1)]
wall R    = map (scalePoint levelItemSize) [(wR - wT, 0), (wR - wT, 1), (wR, 0), (wR, 1)]

wall CcDL = map (scalePoint levelItemSize) $ curvedWall (0, 0) (1 - wR) 0 (pi / 2)
wall CcDR = map (scalePoint levelItemSize) $ curvedWall (1, 0) (1 - wR) (pi / 2) pi
wall CcUR = map (scalePoint levelItemSize) $ curvedWall (1, 1) (1 - wR) pi (3 * pi / 2)
wall CcUL = map (scalePoint levelItemSize) $ curvedWall (0, 1) (1 - wR) (3 * pi / 2) (2 * pi)

wall CvUL = map (scalePoint levelItemSize) $ curvedWall (1, 0) (wR - wT) (pi / 2) pi
wall CvUR = map (scalePoint levelItemSize) $ curvedWall (0, 0) (wR - wT) 0 (pi / 2)
wall CvDL = map (scalePoint levelItemSize) $ curvedWall (1, 1) (wR - wT) pi (3 * pi / 2)
wall CvDR = map (scalePoint levelItemSize) $ curvedWall (0, 1) (wR - wT) (3 * pi / 2) (2 * pi)
wall _    = []

ghwT :: Float
ghwT = 0.3

ghWall :: GHWallDirection -> [(Float, Float)]
ghWall UL = map (scalePoint levelItemSize) [(0.5 - ghwT / 2, 0), (0.5 + ghwT / 2, 0), (0.5 - ghwT / 2, 0.5 + ghwT / 2), (0.5 + ghwT / 2, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2), (1, 0.5 - ghwT / 2)]
ghWall UR = map (scalePoint levelItemSize) [(0, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0.5 - ghwT / 2), (0.5 + ghwT / 2, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0), (0.5 + ghwT / 2, 0)]
ghWall DL = map (scalePoint levelItemSize) [(1, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 0.5 - ghwT / 2), (0.5 + ghwT / 2, 0.5 + ghwT / 2), (0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 1)]
ghWall DR = map (scalePoint levelItemSize) [(0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 1), (0.5 - ghwT / 2, 0.5 + ghwT / 2), (0.5 + ghwT / 2, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (0, 0.5 - ghwT / 2)]
ghWall H  = map (scalePoint levelItemSize) [(0, 0.5 - ghwT / 2), (0, 0.5 + ghwT / 2), (1, 0.5 - ghwT / 2), (1, 0.5 + ghwT / 2)]
ghWall V  = map (scalePoint levelItemSize) [(0.5 - ghwT / 2, 0), (0.5 - ghwT / 2, 1), (0.5 + ghwT / 2, 0), (0.5 + ghwT / 2, 1)]

ghgT :: Float
ghgT = 0.1

ghGate ::  [(Float, Float)]
ghGate = map (scalePoint levelItemSize) [(0, 0.5 - ghgT / 2), (0, 0.5 + ghgT / 2), (1, 0.5 - ghgT / 2), (1, 0.5 + ghgT / 2)]

renderPill :: Float -> (Float, Float) -> IO ()
renderPill r dest = do
    let
        --Position the pill halfway into the level tile
        m = levelItemSize / 2.0
        points = map (translatePoint (m, m) . circlePt r) [0, pi/8..2*pi]

        --Position the level tile in the level
        positionedPoints = map (translatePoint dest) points

        verts = map pointToVertex positionedPoints

    color white
    renderPrimitive TriangleFan $
        mapM_ vertex verts
