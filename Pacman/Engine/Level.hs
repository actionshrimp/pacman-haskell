module Pacman.Engine.Level (levelUpdate) where

import qualified Data.Map as M

import Pacman.Level
import Pacman.Actor

levelUpdate :: [Actor] -> Level -> Level
levelUpdate actors prev = Level {
    levelWidth = levelWidth prev,
    levelHeight = levelHeight prev,
    levelItems = newLevelItems
} where
    pacman = actorWithId Pacman actors
    pacmanPos = actorSrc pacman
    newLevelItems = M.adjust (\_ -> Blank) pacmanPos (levelItems prev)
