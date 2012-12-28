module Pacman.Graphics.Vertex (pointToVertex, translatePoint, scalePoint, circlePt) where

import Control.Arrow

import Graphics.Rendering.OpenGL

pointToVertex :: (Float, Float) -> Vertex3 GLfloat
pointToVertex (x, y) = Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat

circlePt :: Float -> Float -> (Float, Float)
circlePt r t = (r * cos t, r * sin t)

translatePoint :: (Float, Float) -> (Float, Float) -> (Float, Float)
translatePoint (x, y) = (+ x) *** (+ y)

scalePoint :: Float -> (Float, Float) -> (Float, Float)
scalePoint r = (* r) *** (* r)
