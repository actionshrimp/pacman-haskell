module Pacman.Actors.Types.Level (Level, LevelItem, LevelItemT(..), PickupType(..), WallDirection(..), levelItemSize) where

data PickupType = Pill | PowerPill | Cherry
data WallDirection = C | U | D | L | R | 
                     CcUL | CcUR | CcDR | CcDL | --Concave corners
                     CvUL | CvUR | CvDR | CvDL   --Convev corners

data LevelItemT a b = Blank | Pickup a | Wall b | GhostsWall | GhostsGate
type LevelItem = LevelItemT PickupType WallDirection

type Level = [[LevelItem]]

levelItemSize :: Float
levelItemSize = 25
