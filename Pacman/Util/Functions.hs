module Pacman.Util.Functions (cyclingParam, circlePt, translateItem, scaleItem) where

import Control.Arrow

import Pacman.Util.Types.Vec2

translate :: Vec2 -> Vec2 -> Vec2
translate (x, y) = (+ x) *** (+ y)

translateItem :: Vec2 -> [Vec2] -> [Vec2]
translateItem dst = map $ translate dst

scale :: Float -> Vec2 -> Vec2
scale r = (* r) *** (* r)

scaleItem :: Float -> [Vec2] -> [Vec2]
scaleItem r = map $ scale r

circlePt :: Float -> Float -> Vec2
circlePt r t = (r * cos t, r * sin t)

cyclingParam :: (Ord a, Num a) => a -> a -> a -> a
cyclingParam t lowerBound upperBound | t > upperBound = t - (upperBound - lowerBound)
                                     | otherwise = t
