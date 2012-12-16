module Pacman.Util.Functions (cyclingParam, circlePt, translatePts) where

import Control.Arrow

translate :: (Float, Float) -> (Float, Float) -> (Float, Float)
translate (x, y) = (+ x) *** (+ y)

translatePts :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
translatePts dst = map $ translate dst

circlePt :: Float -> Float -> (Float, Float)
circlePt r t = (r * cos t, r * sin t)

cyclingParam :: (Ord a, Num a) => a -> a -> a -> a
cyclingParam t lowerBound upperBound | t > upperBound = t - (upperBound - lowerBound)
                                     | otherwise = t
