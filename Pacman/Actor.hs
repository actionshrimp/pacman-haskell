module Pacman.Actor (ActorId, ActorIdT(..), GhostId(..), Actor(..)) where

import Pacman.Util.Coords

data ActorIdT a = Pacman | Ghost a
data GhostId = GhostA | GhostB | GhostC | GhostD

type ActorId = ActorIdT GhostId

--An actor is moving from coord Src to coord Dst.
--The MoveParam is how far from Src to Dst they have reached (0.5 is halfway there).
--Src and Dst coords are right next to each other
data Actor = Actor {
    actorId :: ActorId,
    actorSrc :: Coords, 
    actorDst :: Coords,
    actorMoveParam :: Float
}
