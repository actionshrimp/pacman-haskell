module Pacman.Actors.Level (
    levelItemSize, 
    levelItem, 
    loadLevel, 
    readLevelData, 
    initialActorPosition,
    levelW, 
    levelH, 
    isTraversable,
    vecToItem) where

import Pacman.Util.Types.Vec2
import Pacman.Util.Types.Direction

import Pacman.Actors.Types.Level

readLevelData :: String -> IO [String]
readLevelData levelName = do
        contents <- readFile ("data/levels/" ++ levelName ++ ".txt")
        return (reverse . lines $ contents)

loadLevel :: [String] -> Level
loadLevel = deriveLevelWallDirections . toLevelItems

toLevelItems :: [String] -> Level
toLevelItems = map (map toLevelItem)

toLevelItem :: Char -> LevelItem
toLevelItem 'x' = Wall C
toLevelItem '.' = Pickup Pill
toLevelItem 'o' = Pickup PowerPill
toLevelItem '-' = GHWall H
toLevelItem '+' = GHGate
toLevelItem  _  = Blank

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
                            item = levelItem lvl  Blank   x     y    ,
                            nU  =  levelItem lvl (Wall C) x     (y+1),
                            nL  =  levelItem lvl (Wall C) (x-1) y    ,
                            nR  =  levelItem lvl (Wall C) (x+1) y    ,
                            nD  =  levelItem lvl (Wall C) x     (y-1),
                            nUL =  levelItem lvl (Wall C) (x-1) (y+1),
                            nUR =  levelItem lvl (Wall C) (x+1) (y+1),
                            nDR =  levelItem lvl (Wall C) (x+1) (y-1),
                            nDL =  levelItem lvl (Wall C) (x-1) (y-1)
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

deriveWallDirection LevelItemWithNeighbours {
    item = GHWall H, nU = GHWall H, nD = GHWall H } = GHWall V
deriveWallDirection LevelItemWithNeighbours {
    item = GHWall H, nL = GHWall H, nD = GHWall H } = GHWall UR
deriveWallDirection LevelItemWithNeighbours {
    item = GHWall H, nR = GHWall H, nD = GHWall H } = GHWall UL
deriveWallDirection LevelItemWithNeighbours {
    item = GHWall H, nL = GHWall H, nU = GHWall H } = GHWall DR
deriveWallDirection LevelItemWithNeighbours {
    item = GHWall H, nR = GHWall H, nU = GHWall H } = GHWall DL

deriveWallDirection i = item i

isTraversable :: LevelItem -> Bool
isTraversable (Wall _) = False
isTraversable (GHWall _) = False
isTraversable GHGate = False
isTraversable _ = True

vecToItem :: Level -> Vec2 -> LevelItem
vecToItem lvl (vecX, vecY) = levelItem lvl Blank x y where
                        (x, y) = (floor (vecX / levelItemSize), floor (vecY / levelItemSize))

initialActorPosition :: [String] -> Char -> Vec2
initialActorPosition levelData actorChar = (levelItemSize * avgX + levelItemSize / 2, levelItemSize * avgY + levelItemSize / 2) where
    avgX = fromIntegral (sum . map fst $ matchingCoords) / fromIntegral (length matchingCoords)
    avgY = fromIntegral (sum . map snd $ matchingCoords) / fromIntegral (length matchingCoords)
    matchingCoords = filter ((== actorChar) . (uncurry (levelItem levelData ' '))) allCoords
    allCoords = [(u, v) | u <- [0..lvlW], v <- [0..lvlH]]
    lvlW = levelW levelData
    lvlH = levelH levelData

