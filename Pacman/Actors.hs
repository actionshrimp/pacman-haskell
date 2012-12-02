module Pacman.Actors (
    Scene(Scene), 
    Direction(Up, Down, Right, Left), 
    Vec2,
    Pacman,
    Ghost
    )
    where
import Data.Int

data Scene = Scene { width :: Int,
                     height :: Int,
                     pacman :: Pacman,
                     ghosts :: [Ghost]
                   }

data Direction = Up | Down | Right | Left
type Vec2 = (Float, Float)

data Pacman = Pacman { position :: Vec2,
                       mouthAngle :: Float,
                       direction :: Direction,
                       queuedDirection :: Maybe Direction }

data Ghost = Ghost { position :: Vec2,
                     direction :: Direction }
