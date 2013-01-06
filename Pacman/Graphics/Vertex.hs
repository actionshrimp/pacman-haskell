module Pacman.Graphics.Vertex (pointToVertex, translatePoint, scalePoint, circlePt, actorPositionPoint) where

import Control.Arrow

import Pacman.Util.Coords

import Pacman.Actor

import Graphics.Rendering.OpenGL

actorPositionPoint :: Float -> Actor -> (Float, Float)
actorPositionPoint levelItemSize a = p where
    srcCoords = actorSrc a
    dstCoords = actorDst a
    direcCoords = direcVecCoords srcCoords dstCoords
    moveParam = actorMoveParam a
    coords = translatePoint (scaleCoords moveParam direcCoords) (scaleCoords 1 srcCoords)
    p = scalePoint levelItemSize coords

pointToVertex :: (Float, Float) -> Vertex3 GLfloat
pointToVertex (x, y) = Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat

circlePt :: Float -> Float -> (Float, Float)
circlePt r t = (r * cos t, r * sin t)

translatePoint :: (Float, Float) -> (Float, Float) -> (Float, Float)
translatePoint (x, y) = (+ x) *** (+ y)

scalePoint :: Float -> (Float, Float) -> (Float, Float)
scalePoint r = (* r) *** (* r)
