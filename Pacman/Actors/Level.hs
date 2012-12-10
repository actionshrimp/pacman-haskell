module Pacman.Actors.Level (loadLevel, loadLevelData) where

import Pacman.Actors.Types.Pickups
import Pacman.Actors.Types.Level

sceneItem :: Char -> SceneItem PickupType
sceneItem ' ' = Blank
sceneItem 'x' = Wall
sceneItem '.' = Pickup Pill
sceneItem 'o' = Pickup PowerPill
sceneItem '-' = GhostsWall
sceneItem '+' = GhostsGate
sceneItem _ = Blank

loadLevel :: [String] -> Level
loadLevel = map (map sceneItem)

loadLevelData :: String -> IO [String]
loadLevelData levelName = do
        contents <- readFile ("data/levels/" ++ levelName ++ ".txt")
        return (lines contents)
