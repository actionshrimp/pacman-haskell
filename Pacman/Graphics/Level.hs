module Pacman.Graphics.Level (renderLevel) where

import Control.Monad

import Graphics.Rendering.OpenGL

import Pacman.Graphics.Base

import Pacman.Actors.Types.Pickups
import Pacman.Actors.Types.Level as Level

import Pacman.Actors.Level

data ItemWithNeighbours = ItemWithNeighbours { middle :: LevelItem PickupType,
                                       nbrU :: LevelItem PickupType,
                                       nbrL :: LevelItem PickupType,
                                       nbrD :: LevelItem PickupType,
                                       nbrR :: LevelItem PickupType }

itemWithNeighbours :: Level.Level -> Int -> Int -> ItemWithNeighbours
itemWithNeighbours lvl x y = ItemWithNeighbours {
                            middle = levelItem lvl x y,
                            nbrU = levelItem lvl x (y-1),
                            nbrL = levelItem lvl (x-1) y,
                            nbrR = levelItem lvl (x+1) y,
                            nbrD = levelItem lvl x (y+1)
                            }

data WallDirection = NoWall | WallL | WallU | WallR | WallD |
                     WallUL | WallUR | WallDL | WallDR | WallSolid deriving (Eq)

wallDirection :: ItemWithNeighbours -> WallDirection
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = Wall, nbrD = Wall, nbrR = Wall} = WallSolid
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = _, nbrD = Wall, nbrR = Wall} = WallL
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = _, nbrL = Wall, nbrD = Wall, nbrR = Wall} = WallU
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = Wall, nbrD = Wall, nbrR = _} = WallR
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = Wall, nbrD = _, nbrR = Wall} = WallD
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = _, nbrL = _, nbrD = Wall, nbrR = Wall} = WallUL
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = _, nbrL = Wall, nbrD = Wall, nbrR = _} = WallUR
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = _, nbrD = _, nbrR = Wall} = WallDL
wallDirection ItemWithNeighbours {
    middle = Wall, nbrU = Wall, nbrL = Wall, nbrD = _, nbrR = _} = WallDR
wallDirection ItemWithNeighbours {
    middle = _, nbrU = _, nbrL = _, nbrD = _, nbrR = _} = NoWall

wallDirections :: Level.Level -> [[WallDirection]]
wallDirections lvl = map (map wallDirection) (mapNeighbours lvl) where
                    mapNeighbours = foldl overRows []
                    overRows acc row = acc ++ [foldl (overCols (length acc)) [] row]
                    overCols y acc _ = acc ++ [itemWithNeighbours lvl (length acc) y]

blue :: Color3 GLfloat
blue = (Color3 0 0 1 :: Color3 GLfloat)

renderLevel :: Level.Level -> IO ()
renderLevel lvl = do
    let xs = map (*levelItemSize) [0.. length (head lvl)]
        ys = reverse $ map (*levelItemSize) [0.. length lvl]
    
        coordsGrid = map (\y -> zip xs (repeat y)) ys

        pairedGrid = zipWith (zipWith (\x y -> (x, y))) (wallDirections lvl) coordsGrid

    mapM_ (mapM_ renderWall) pairedGrid 

renderWall :: (WallDirection, (Int, Int)) -> IO ()
renderWall (wallDir, (iX, iY)) = 
    unless (wallDir == NoWall) $ do
            let 
                x = fromIntegral iX
                y = fromIntegral iY
                w = fromIntegral levelItemSize
                wallPoints = [(x, y - w), (x, y), (x + w, y - w), (x + w, y)]

                wallVerts = map pointToVertex wallPoints

            color blue
            renderPrimitive TriangleStrip $
                mapM_ vertex wallVerts
