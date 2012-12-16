module Pacman.Util.Types.Direction (Direction(..), directionVec) where

import Pacman.Util.Types.Vec2

data Direction = DUp | DDown | DRight | DLeft

directionVec :: Direction -> Vec2
directionVec DUp = (0, 1)
directionVec DDown = (0, -1)
directionVec DRight = (1, 0)
directionVec DLeft = (-1, 0)
