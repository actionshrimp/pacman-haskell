module Pacman.Util.Types.InputCommands (InputCommand, InputCommandT(..)) where

import Pacman.Util.Types.Direction

data InputCommandT a = MovePacman a | Exit | OpenMenu
type InputCommand = InputCommandT Direction
