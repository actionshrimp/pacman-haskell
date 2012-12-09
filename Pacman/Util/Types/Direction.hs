module Pacman.Util.Types.Direction (Direction(..), directionVec) where

import Pacman.Util.Types.Vec2

data Direction = DUp | DDown | DRight | DLeft

directionVec :: Direction -> Vec2
directionVec DUp = Vec2 {x = 0, y = 1}
directionVec DDown = Vec2 {x = 0, y = -1}
directionVec DRight = Vec2 {x = 1, y = 0}
directionVec DLeft = Vec2 {x = -1, y = 0}
