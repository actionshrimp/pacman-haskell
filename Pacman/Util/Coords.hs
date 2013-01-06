module Pacman.Util.Coords (Coords, translateCoords, scaleCoords, negateCoords, direcVecCoords) where

import Control.Arrow

type Coords = (Int, Int)

translateCoords :: Coords -> Coords -> Coords
translateCoords (x, y) = (+ x) *** (+ y)

scaleCoords :: Float -> Coords -> (Float, Float)
scaleCoords r = ((*r) . fromIntegral) *** ((*r) . fromIntegral)

negateCoords :: Coords -> Coords
negateCoords (x, y) = (negate x, negate y)

direcVecCoords :: Coords -> Coords -> Coords
direcVecCoords src dst | abs baseX > 1 = (negate baseX `div` abs baseX, baseY)
                       | otherwise = (baseX, baseY) where
    (baseX, baseY) = translateCoords dst (negateCoords src)
