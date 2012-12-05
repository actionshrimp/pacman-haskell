module Pacman.Actors.Base (Vec2, Direction(..), cyclingParam) where

type Vec2 = (Float, Float)
data Direction = DUp | DDown | DRight | DLeft

cyclingParam :: (Ord a, Num a) => a -> a -> a -> a
cyclingParam t lowerBound upperBound | t > upperBound = t - (upperBound - lowerBound)
                                     | otherwise = t
