module Pacman.Actors.Types.Level (Level, LevelItem, LevelItemT(..), PickupType(..), WallDirection(..), GHWallDirection(..), levelItemSize) where

data PickupType = Pill | PowerPill | Cherry
data WallDirection = C | U | D | L | R | 
                     CcUL | CcUR | CcDR | CcDL | --Concave corners
                     CvUL | CvUR | CvDR | CvDL   --Convev corners

data GHWallDirection = H | V | UL | UR | DL | DR

data LevelItemT a b c = Blank | Pickup a | Wall b | GHWall c | GHGate
type LevelItem = LevelItemT PickupType WallDirection GHWallDirection

type Level = [[LevelItem]]

levelItemSize :: Float
levelItemSize = 26
