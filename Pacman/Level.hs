module Pacman.Level (
    PickupType(..),
    WallDirection(..),
    GHWallDirection(..),
    LevelItemT(..),
    LevelItem,
    Level(..),
    loadLevel,
    isTraversable
) where

import qualified Data.Map as M
import Data.Maybe

import Pacman.Util.Coords

data PickupType = Pill | PowerPill | Cherry

data WallDirection = C | U | D | L | R | 
                     CcUL | CcUR | CcDR | CcDL | --Concave corners
                     CvUL | CvUR | CvDR | CvDL   --Convex corners

data GHWallDirection = H | V | UL | UR | DL | DR

data LevelItemT a b c = Blank | Pickup a | Wall b | GHWall c | GHGate

type LevelItem = LevelItemT PickupType WallDirection GHWallDirection

data Level = Level {
    levelWidth :: Int,
    levelHeight :: Int,
    levelItems :: M.Map Coords LevelItem
}
loadLevel :: [String] -> Level
loadLevel levelData = Level {
    levelWidth = w,
    levelHeight = h,
    levelItems = deriveDetail . toLevelItems $ levelData
} where
    w = length . head $ levelData
    h = length levelData

toLevelItems :: [String] -> M.Map Coords LevelItem
toLevelItems = M.fromList . rowsToLevelItems . map rowToLevelItem

rowToLevelItem :: String -> [(Int, LevelItem)]
rowToLevelItem = foldl (\acc i -> (length acc, toLevelItem i) : acc) []

rowsToLevelItems :: [[(Int, LevelItem)]] -> [(Coords, LevelItem)]
rowsToLevelItems = concat . foldl f [] where
    f acc row = map (addRow (length acc)) row : acc
    addRow row (col, i) = ((col, row), i)

toLevelItem :: Char -> LevelItem
toLevelItem 'x' = Wall C
toLevelItem '.' = Pickup Pill
toLevelItem 'o' = Pickup PowerPill
toLevelItem '-' = GHWall H
toLevelItem '+' = GHGate
toLevelItem  _  = Blank

deriveDetail :: M.Map Coords LevelItem -> M.Map Coords LevelItem
deriveDetail = deriveWallDirections

deriveWallDirections :: M.Map Coords LevelItem -> M.Map Coords LevelItem
deriveWallDirections items = M.mapWithKey (deriveWallDirection items) items

data LevelItemWithNeighbours = LevelItemWithNeighbours { item :: LevelItem,
                                                         nU   :: LevelItem,
                                                         nL   :: LevelItem,
                                                         nD   :: LevelItem,
                                                         nR   :: LevelItem,
                                                         nUL  :: LevelItem,
                                                         nUR  :: LevelItem,
                                                         nDR  :: LevelItem,
                                                         nDL  :: LevelItem }

deriveWallDirection :: M.Map Coords LevelItem -> Coords -> LevelItem -> LevelItem
deriveWallDirection items (x, y) i = wallDirectionFromNeighbours LevelItemWithNeighbours {
        item = i,
        nU  = getItem (x    , y + 1) items,
        nR  = getItem (x + 1, y    ) items,
        nL  = getItem (x - 1, y    ) items,
        nD  = getItem (x    , y - 1) items,
        nUR = getItem (x + 1, y + 1) items,
        nUL = getItem (x - 1, y + 1) items,
        nDR = getItem (x + 1, y - 1) items,
        nDL = getItem (x - 1, y - 1) items
} where getItem = M.findWithDefault (Wall C) 

wallDirectionFromNeighbours :: LevelItemWithNeighbours -> LevelItem
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR = Wall C, nDL = Wall C} = Wall C
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL =      _, nUR = Wall C, nDR = Wall C, nDL = Wall C} = Wall CcUL
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR =      _, nDR = Wall C, nDL = Wall C} = Wall CcUR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR =      _, nDL = Wall C} = Wall CcDR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD = Wall C, nUL = Wall C, nUR = Wall C, nDR = Wall C, nDL =      _} = Wall CcDL
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL = Wall C, nR = Wall C, nD = Wall C, nUL =      _, nUR =      _, nDR = Wall C, nDL = Wall C} = Wall U
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR = Wall C, nD =      _, nUL = Wall C, nUR = Wall C, nDR =      _, nDL =      _} = Wall D
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL =      _, nR = Wall C, nD = Wall C, nUL =      _, nUR = Wall C, nDR = Wall C, nDL =      _} = Wall L
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR =      _, nD = Wall C, nUL = Wall C, nUR =      _, nDR =      _, nDL = Wall C} = Wall R
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL =      _, nR = Wall C, nD = Wall C, nUL =      _, nUR =      _, nDR = Wall C, nDL =      _} = Wall CvUL
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU =      _, nL = Wall C, nR =      _, nD = Wall C, nUL =      _, nUR =      _, nDR =      _, nDL = Wall C} = Wall CvUR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL = Wall C, nR =      _, nD =      _, nUL = Wall C, nUR =      _, nDR =      _, nDL =      _} = Wall CvDR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = Wall C, nU = Wall C, nL =      _, nR = Wall C, nD =      _, nUL =      _, nUR = Wall C, nDR =      _, nDL =      _} = Wall CvDL

wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = GHWall H, nU = GHWall H, nD = GHWall H } = GHWall V
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = GHWall H, nL = GHWall H, nD = GHWall H } = GHWall UR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = GHWall H, nR = GHWall H, nD = GHWall H } = GHWall UL
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = GHWall H, nL = GHWall H, nU = GHWall H } = GHWall DR
wallDirectionFromNeighbours LevelItemWithNeighbours {
    item = GHWall H, nR = GHWall H, nU = GHWall H } = GHWall DL
wallDirectionFromNeighbours i = item i

isTraversableItem :: LevelItem -> Bool
isTraversableItem (Wall _) = False
isTraversableItem (GHWall _) = False
isTraversableItem GHGate = False
isTraversableItem _ = True

isTraversable :: Level -> Coords -> Bool
isTraversable level coords = isTraversableItem i where
    i = fromMaybe (Wall C) (M.lookup coords (levelItems level))
