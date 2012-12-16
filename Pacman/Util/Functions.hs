module Pacman.Util.Functions (cyclingParam, circlePt, translateItem, scaleItem) where

import Control.Arrow

translate :: (Float, Float) -> (Float, Float) -> (Float, Float)
translate (x, y) = (+ x) *** (+ y)

translateItem :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
translateItem dst = map $ translate dst

scale :: Float -> (Float, Float) -> (Float, Float)
scale r = (* r) *** (* r)

scaleItem :: Float -> [(Float, Float)] -> [(Float, Float)]
scaleItem r = map $ scale r

circlePt :: Float -> Float -> (Float, Float)
circlePt r t = (r * cos t, r * sin t)

cyclingParam :: (Ord a, Num a) => a -> a -> a -> a
cyclingParam t lowerBound upperBound | t > upperBound = t - (upperBound - lowerBound)
                                     | otherwise = t
