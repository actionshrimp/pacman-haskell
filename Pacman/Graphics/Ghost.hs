module Pacman.Graphics.Ghost (renderGhost) where

import Graphics.Rendering.OpenGL

import Pacman.Graphics.Base
import qualified Pacman.Actors.Ghost as Ghost

renderGhost :: Ghost.Ghost -> Int -> IO ()
renderGhost ghost i = do
    let
        (x, y) = Ghost.position ghost

        r = 20
        fanPoints = (x, y) : map (\a -> (x + r * cos a, y + r * sin a)) [0, pi/64..pi]
        fanVertices = map pointToVertex fanPoints

        recPoints = [(x - r, y), (x - r, y-(r/2)), (x+r, y), (x+r, y-(r/2))]
        recVertices = map pointToVertex recPoints

        frillXSamples = [(x - r), (x - r)+(2*r / 20)..(x+r)]
        frillBaseline = zip frillXSamples (repeat (y-(r/2)))

        frillWave t u = y - (r/2) - (r/2) * sin ((pi * u / (2 * r / 3)) - (4 * t)) ^ (2 :: Integer)

        frillPeaks = zip frillXSamples (map (frillWave (pi * Ghost.wobbleParam ghost)) frillXSamples)
        frillPeaksFade = zip frillXSamples (map (frillWave (negate pi * Ghost.wobbleParam ghost)) frillXSamples)

        frillPoints = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaks
        frillPointsFade = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaksFade

        frillVertices = map pointToVertex frillPoints
        frillVerticesFade = map pointToVertex frillPointsFade

        outlineR = r / 2.2
        whiteR = r / 2.5
        spaceX = r / 2.9
        offX = r / 10 
        offY = r / 3
        
        lEyeOutline = (x - spaceX + offX, y + offY) : map (\a -> ((x - spaceX + offX) + (7 * outlineR / 8) * cos a, y + offY + outlineR * sin a)) [0, pi/16..2*pi]
        rEyeOutline = map (\a -> ((x + spaceX + offX) + (7 * outlineR / 8) * cos a, y + offY + outlineR * sin a)) [0, pi/16..2*pi]
        lEyeOutlineVertices = map pointToVertex lEyeOutline
        rEyeOutlineVertices = map pointToVertex rEyeOutline
 
        lEyeWhite = map (\a -> ((x - spaceX + offX) + (7 * whiteR / 8) * cos a, y + offY + whiteR * sin a)) [0, pi/16..2*pi]
        rEyeWhite = map (\a -> ((x + spaceX + offX) + (7 * whiteR / 8) * cos a, y + offY + whiteR * sin a)) [0, pi/16..2*pi]
        lEyeWhiteVertices = map pointToVertex lEyeWhite
        rEyeWhiteVertices = map pointToVertex rEyeWhite

        pupilR = r / 6
        pupilOffX = r / 4
        lPupil = map (\a -> ((x - spaceX + pupilOffX) + pupilR * cos a, y + offY + pupilR * sin a)) [0, pi/16..2*pi]
        rPupil = map (\a -> ((x + spaceX + pupilOffX) + pupilR * cos a, y + offY + pupilR * sin a)) [0, pi/16..2*pi]
        lPupilVertices = map pointToVertex lPupil
        rPupilVertices = map pointToVertex rPupil

        drawColor = case i of 
            1 -> Color3 1  0   0 :: Color3 GLfloat
            2 -> Color3 1  0.4 1 :: Color3 GLfloat
            3 -> Color3 0  1   1 :: Color3 GLfloat
            4 -> Color3 1  0.6 0 :: Color3 GLfloat

        fadeColor = case i of
            1 -> Color3 0.5  0   0   :: Color3 GLfloat
            2 -> Color3 0.5  0.2 0.5 :: Color3 GLfloat
            3 -> Color3 0    0.5 0.5 :: Color3 GLfloat
            4 -> Color3 0.5  0.3 0   :: Color3 GLfloat

        white = (Color3 1 1 1 :: Color3 GLfloat)
        black = (Color3 0 0 0 :: Color3 GLfloat)

    color fadeColor

    renderPrimitive TriangleStrip $
        mapM_ vertex frillVerticesFade

    color drawColor

    renderPrimitive TriangleFan $
        mapM_ vertex fanVertices

    renderPrimitive TriangleStrip $
        mapM_ vertex recVertices

    renderPrimitive TriangleStrip $
        mapM_ vertex frillVertices

    color black

    renderPrimitive TriangleFan $
        mapM_ vertex lEyeOutlineVertices

    renderPrimitive TriangleFan $
        mapM_ vertex rEyeOutlineVertices

    color white

    renderPrimitive TriangleFan $
        mapM_ vertex lEyeWhiteVertices

    renderPrimitive TriangleFan $
        mapM_ vertex rEyeWhiteVertices

    color black

    renderPrimitive TriangleFan $
        mapM_ vertex lPupilVertices

    renderPrimitive TriangleFan $
        mapM_ vertex rPupilVertices
