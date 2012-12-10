module Pacman.Actors.Types.Level (Level, SceneItem(..)) where

import Pacman.Actors.Types.Pickups

type Level = [[SceneItem PickupType]]
data SceneItem a = Blank | Pickup a | Wall | GhostsWall | GhostsGate
