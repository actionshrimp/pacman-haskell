module Pacman.Actors.Types.Level (Level, LevelItem, LevelItemT(..), PickupType(..), WallDirection(..), GHWallDirection(..), levelItemSize, actorV, levelItem, levelW, levelH) where

data PickupType = Pill | PowerPill | Cherry
data WallDirection = C | U | D | L | R | 
                     CcUL | CcUR | CcDR | CcDL | --Concave corners
                     CvUL | CvUR | CvDR | CvDL   --Convex corners

data GHWallDirection = H | V | UL | UR | DL | DR

data LevelItemT a b c = Blank | Pickup a | Wall b | GHWall c | GHGate
type LevelItem = LevelItemT PickupType WallDirection GHWallDirection

type Level = [[LevelItem]]

levelItemSize :: Float
levelItemSize = 26

actorV :: Float
actorV = levelItemSize * 4

levelItem :: [[a]] -> a -> Int -> Int -> a
levelItem lvl defaultItem x y 
    | x >= 0 && x < lvlW && y >= 0 && y < lvlH = (lvl !! y) !! x
    | otherwise = defaultItem
    where lvlW = levelW lvl
          lvlH = levelH lvl

levelW :: [[a]] -> Int
levelW = length . head

levelH :: [[a]] -> Int
levelH = length
