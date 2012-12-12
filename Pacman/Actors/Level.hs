module Pacman.Actors.Level (levelItemSize, levelItem, loadLevel, readLevelData) where

import Pacman.Actors.Types.Level

readLevelData :: String -> IO [String]
readLevelData levelName = do
        contents <- readFile ("data/levels/" ++ levelName ++ ".txt")
        return (lines contents)

loadLevel :: [String] -> Level
loadLevel = deriveLevelWallDirections . toLevelItems

toLevelItems :: [String] -> Level
toLevelItems = map (map toLevelItem)

toLevelItem :: Char -> LevelItem
toLevelItem 'x' = Wall C
toLevelItem '.' = Pickup Pill
toLevelItem 'o' = Pickup PowerPill
toLevelItem '-' = GhostsWall
toLevelItem '+' = GhostsGate
toLevelItem  _  = Blank

levelItem :: Level -> Int -> Int -> LevelItem
levelItem lvl x y | x >= 0 && x < lvlW && y >= 0 && y < lvlH = (lvl !! y) !! x
                  | otherwise = Wall C
                  where lvlW = length $ head lvl
                        lvlH = length lvl

deriveLevelWallDirections :: Level -> Level
deriveLevelWallDirections = deriveWallDirections . levelWithNeighbours

levelWithNeighbours :: Level -> [[LevelItemWithNeighbours]]
levelWithNeighbours lvl = foldl overRows [] lvl where
                          overRows acc row = acc ++ [foldl (overCols (length acc)) [] row]
                          overCols y acc _ = acc ++ [levelItemWithNeighbours lvl (length acc) y]

data LevelItemWithNeighbours = LevelItemWithNeighbours { item :: LevelItem,
                                                         nU   :: LevelItem,
                                                         nL   :: LevelItem,
                                                         nD   :: LevelItem,
                                                         nR   :: LevelItem,
                                                         nUL  :: LevelItem,
                                                         nUR  :: LevelItem,
                                                         nDR  :: LevelItem,
                                                         nDL  :: LevelItem }

levelItemWithNeighbours :: Level -> Int -> Int -> LevelItemWithNeighbours
levelItemWithNeighbours lvl x y = LevelItemWithNeighbours {
                            item = levelItem lvl x     y    ,
                            nU  =  levelItem lvl x     (y+1),
                            nL  =  levelItem lvl (x-1) y    ,
                            nR  =  levelItem lvl (x+1) y    ,
                            nD  =  levelItem lvl x     (y-1),
                            nUL =  levelItem lvl (x-1) (y+1),
                            nUR =  levelItem lvl (x+1) (y+1),
                            nDR =  levelItem lvl (x+1) (y-1),
                            nDL =  levelItem lvl (x-1) (y-1)
                            }

deriveWallDirections :: [[LevelItemWithNeighbours]] -> Level
deriveWallDirections = map (map deriveWallDirection)

deriveWallDirection :: LevelItemWithNeighbours -> LevelItem
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR = Wall C, nDL = Wall C} = Wall C
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL =      _, nUR = Wall C, nDR = Wall C, nDL = Wall C} = Wall CcUL
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR =      _, nDR = Wall C, nDL = Wall C} = Wall CcUR
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR =      _, nDL = Wall C} = Wall CcDR
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR = Wall C, nDL =      _} = Wall CcDL
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL = Wall C, nR = Wall C, nD = Wall C, nUL =      _, nUR =      _, nDR = Wall C, nDL = Wall C} = Wall U
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD =      _, nUL = Wall C, nUR = Wall C, nDR =      _, nDL =      _} = Wall D
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL =      _, nR = Wall C, nD = Wall C, nUL =      _, nUR = Wall C, nDR = Wall C, nDL =      _} = Wall L
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR =      _, nD = Wall C, nUL = Wall C, nUR =      _, nDR =      _, nDL = Wall C} = Wall R
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL =      _, nR = Wall C, nD = Wall C, nUL =      _, nUR =      _, nDR = Wall C, nDL =      _} = Wall CvUL
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL = Wall C, nR =      _, nD = Wall C, nUL =      _, nUR =      _, nDR =      _, nDL = Wall C} = Wall CvUR
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR =      _, nD =      _, nUL = Wall C, nUR =      _, nDR =      _, nDL =      _} = Wall CvDR
deriveWallDirection LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL =      _, nR = Wall C, nD =      _, nUL =      _, nUR = Wall C, nDR =      _, nDL =      _} = Wall CvDL
deriveWallDirection i = item i