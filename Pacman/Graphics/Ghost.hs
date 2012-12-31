module Pacman.Graphics.Ghost (renderGhosts) where

import Data.List

import Graphics.Rendering.OpenGL

import Pacman.Util.Coords

import Pacman.World
import Pacman.Actor

import Pacman.Effects
import Pacman.Effects.GhostFrillEffect
import Pacman.Effects.GhostEyesEffect

import Pacman.Graphics.Vertex
import Pacman.Graphics.Level

renderGhosts :: World -> IO ()
renderGhosts world = do
    let
        actors = worldActors world
        effects = worldEffects world
        rG = renderGhost actors effects

    rG GhostA
    rG GhostB
    rG GhostC
    rG GhostD

renderGhost :: [Actor] -> Effects -> GhostId -> IO ()
renderGhost actors effects gId = do
    let
        ghostActor = actorWithId (Ghost gId) actors
        srcCoords = actorSrc ghostActor
        dstCoords = actorDst ghostActor
        direcCoords = direcVecCoords srcCoords dstCoords
        moveParam = actorMoveParam ghostActor
        coords = translatePoint (scaleCoords moveParam direcCoords) (scaleCoords 1 srcCoords)
        (x, y) = scalePoint levelItemSize coords

        frillEffect = ghostFrillEffect effects
        frillParam = ghostFrillEffectValue frillEffect

        Just eyesEffect = find (\x -> ghostEyesEffectGhostId x == gId) (ghostEyesEffects effects)
        eyesPos = ghostEyesEffectPosition eyesEffect

        r = 20 :: Float
        fanPoints = (x, y) : map (\a -> (x + r * cos a, y + r * sin a)) [0, pi/64..pi]
        fanVertices = map pointToVertex fanPoints

        recPoints = [(x - r, y), (x - r, y-(3*r/4)), (x+r, y), (x+r, y-(3*r/4))]
        recVertices = map pointToVertex recPoints

        frillXSamples = [(x - r), (x - r)+(2*r / 20)..(x+r)]
        frillBaseline = zip frillXSamples (repeat (y-(3*r/4)))

        frillWave t u = y - (3*r/4) - (r/4) * sin ((pi * u / (2 * r / 3)) - (4 * t)) ^ (2 :: Integer)

        frillPeaks = zip frillXSamples (map (frillWave (pi * frillParam)) frillXSamples)
        frillPeaksFade = zip frillXSamples (map (frillWave (negate pi * frillParam)) frillXSamples)

        frillPoints = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaks
        frillPointsFade = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaksFade

        frillVertices = map pointToVertex frillPoints
        frillVerticesFade = map pointToVertex frillPointsFade

        outlineR = r / 2.2
        whiteR = r / 2.5
        spaceX = r / 2.9
        offX = (fst eyesPos) * r / 10 
        offY = (r / 6) + (snd eyesPos) * r / 6
        
        lEyeOutline = (x - spaceX + offX, y + offY) : map (\a -> ((x - spaceX + offX) + (7 * outlineR / 8) * cos a, y + offY + outlineR * sin a)) [0, pi/16..2*pi]
        rEyeOutline = map (\a -> ((x + spaceX + offX) + (7 * outlineR / 8) * cos a, y + offY + outlineR * sin a)) [0, pi/16..2*pi]
        lEyeOutlineVertices = map pointToVertex lEyeOutline
        rEyeOutlineVertices = map pointToVertex rEyeOutline
 
        lEyeWhite = map (\a -> ((x - spaceX + offX) + (7 * whiteR / 8) * cos a, y + offY + whiteR * sin a)) [0, pi/16..2*pi]
        rEyeWhite = map (\a -> ((x + spaceX + offX) + (7 * whiteR / 8) * cos a, y + offY + whiteR * sin a)) [0, pi/16..2*pi]
        lEyeWhiteVertices = map pointToVertex lEyeWhite
        rEyeWhiteVertices = map pointToVertex rEyeWhite

        pupilR = r / 6
        pupilOffX = (fst eyesPos) * r / 4
        pupilOffY = (snd eyesPos) * r / 5
        lPupil = map (\a -> ((x - spaceX + pupilOffX) + pupilR * cos a, y + offY + pupilOffY + pupilR * sin a)) [0, pi/16..2*pi]
        rPupil = map (\a -> ((x + spaceX + pupilOffX) + pupilR * cos a, y + offY + pupilOffY + pupilR * sin a)) [0, pi/16..2*pi]
        lPupilVertices = map pointToVertex lPupil
        rPupilVertices = map pointToVertex rPupil

        drawColor = case gId of 
            GhostA -> Color3 1  0   0 :: Color3 GLfloat
            GhostB -> Color3 1  0.4 1 :: Color3 GLfloat
            GhostC -> Color3 0  1   1 :: Color3 GLfloat
            GhostD -> Color3 1  0.6 0 :: Color3 GLfloat

        fadeColor = case gId of
            GhostA -> Color3 0.5  0   0   :: Color3 GLfloat
            GhostB -> Color3 0.5  0.2 0.5 :: Color3 GLfloat
            GhostC -> Color3 0    0.5 0.5 :: Color3 GLfloat
            GhostD -> Color3 0.5  0.3 0   :: Color3 GLfloat

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
