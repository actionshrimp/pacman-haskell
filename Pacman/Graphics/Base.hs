module Pacman.Graphics.Base (pointToVertex) where

import Graphics.Rendering.OpenGL

pointToVertex :: (Float, Float) -> Vertex3 GLfloat
pointToVertex (x, y) = Vertex3 (realToFrac x) (realToFrac y) 0  :: Vertex3 GLfloat
