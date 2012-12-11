    module Pacman.Actors.Types.Level (Level, LevelItem(..)) where

import Pacman.Actors.Types.Pickups

type Level = [[LevelItem PickupType]]
data LevelItem a = Blank | Pickup a | Wall | GhostsWall | GhostsGate
