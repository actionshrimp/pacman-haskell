module Pacman.Actors.Level (levelItemSize, levelItem, loadLevel, loadLevelData) where

import Pacman.Actors.Types.Pickups
import Pacman.Actors.Types.Level

levelItemSize :: Int
levelItemSize = 25

levelItem :: Level -> Int -> Int -> LevelItem PickupType
levelItem lvl x y | x >= 0 && x < lvlW && y >= 0 && y < lvlH = (lvl !! y) !! x
                  | otherwise = Wall
                  where lvlW = length $ head lvl
                        lvlH = length lvl

levelDataToLevelItem :: Char -> LevelItem PickupType
levelDataToLevelItem ' ' = Blank
levelDataToLevelItem 'x' = Wall
levelDataToLevelItem '.' = Pickup Pill
levelDataToLevelItem 'o' = Pickup PowerPill
levelDataToLevelItem '-' = GhostsWall
levelDataToLevelItem '+' = GhostsGate
levelDataToLevelItem _ = Blank

loadLevel :: [String] -> Level
loadLevel = map (map levelDataToLevelItem)

loadLevelData :: String -> IO [String]
loadLevelData levelName = do
        contents <- readFile ("data/levels/" ++ levelName ++ ".txt")
        return (lines contents)
