module Pacman.Util.Functions (cyclingParam, circlePt) where

circlePt :: Float -> Float -> (Float, Float)
circlePt r t = (r * cos t, r * sin t)

cyclingParam :: (Ord a, Num a) => a -> a -> a -> a
cyclingParam t lowerBound upperBound | t > upperBound = t - (upperBound - lowerBound)
                                     | otherwise = t
