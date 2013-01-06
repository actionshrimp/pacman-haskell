module Pacman.Graphics.Ghost (renderGhosts) where

import Data.List
import Data.Maybe

import Graphics.Rendering.OpenGL

import Pacman.Util.Coords

import Pacman.World
import Pacman.Actor
import Pacman.GameState

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
        states = worldStates world
        rG = renderGhost actors effects states

    rG GhostA
    rG GhostB
    rG GhostC
    rG GhostD

activatingStatePositionTranslation :: [GameState] -> GhostId -> (Float, Float)
activatingStatePositionTranslation states gId = (0, -transY) where
    maybeActivatingState = find (\t -> gameStateId t == (GhostActivating gId)) states
    activatingState = fromMaybe zeroTimeState maybeActivatingState
    activatingTime = gameStateRemainingTime activatingState
    transY = levelItemSize * actorVelocity * activatingTime

waitingStatePositionTranslation :: [GameState] -> GhostId -> (Float, Float)
waitingStatePositionTranslation states gId = (transX, -transY) where
    maybeActivatingState = find (\t -> gameStateId t == (GhostWaiting gId)) states
    activatingState = fromMaybe zeroTimeState  maybeActivatingState
    activatingTime = gameStateRemainingTime activatingState
    transX | gId == GhostC = (min 0.5 activatingTime) * actorVelocity * levelItemSize * (-1)
           | gId == GhostD = (min 0.5 activatingTime) * actorVelocity * levelItemSize
           | otherwise = 0
    transY | activatingTime > 0 = levelItemSize * actorVelocity * 0.75
           | otherwise = 0

renderGhost :: [Actor] -> Effects -> [GameState] -> GhostId -> IO ()
renderGhost actors effects states gId = do
    let
        ghostActor = actorWithId (Ghost gId) actors
        --srcCoords = actorSrc ghostActor
        --dstCoords = actorDst ghostActor
        --direcCoords = direcVecCoords srcCoords dstCoords
        --moveParam = actorMoveParam ghostActor
        --coords = translatePoint (scaleCoords moveParam direcCoords) (scaleCoords 1 srcCoords)
        basePosition = actorPositionPoint levelItemSize ghostActor

        activatingTranslation = activatingStatePositionTranslation states gId
        waitingTranslation = waitingStatePositionTranslation states gId

        activatingPosition = translatePoint basePosition activatingTranslation
        (x, y) = translatePoint activatingPosition waitingTranslation

        frillEffect = ghostFrillEffect effects
        frillParam = ghostFrillEffectValue frillEffect

        Just eyesEffect = find (\t -> ghostEyesEffectGhostId t == gId) (ghostEyesEffects effects)
        eyesPos = ghostEyesEffectPosition eyesEffect

        scaleAndPosition = translatePoint (x, y) . scalePoint levelItemSize . translatePoint (0.5, 0.5)

        r = 0.75 :: Float
        fanPoints = (0, 0) : map (\a -> (r * cos a, r * sin a)) [0, pi/64..pi]
        fanVertices = map (pointToVertex . scaleAndPosition) fanPoints

        recPoints = [(-r, 0), (-r, -(3*r/4)), (r, 0), (r, -(3*r/4))]
        recVertices = map (pointToVertex . scaleAndPosition) recPoints

        frillXSamples = [(-r), (-r)+(2 * r / 20)..r]
        frillBaseline = zip frillXSamples (repeat (-3 * r/4))

        frillWave t u = (-3 * r/4) - (r/4) * sin ((pi * u / (2 * r / 3)) - (4 * t)) ^ (2 :: Integer)

        frillPeaks = zip frillXSamples (map (frillWave (pi * frillParam)) frillXSamples)
        frillPeaksFade = zip frillXSamples (map (frillWave (negate pi * frillParam)) frillXSamples)

        frillPoints = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaks
        frillPointsFade = concat $ zipWith (\pX pY -> [pX, pY]) frillBaseline frillPeaksFade

        frillVertices = map (pointToVertex . scaleAndPosition) frillPoints
        frillVerticesFade = map (pointToVertex . scaleAndPosition) frillPointsFade

        outlineR = r / 2.2
        whiteR = r / 2.5
        spaceX = r / 2.9
        offX = fst eyesPos * r / 10 
        offY = (r / 6) + snd eyesPos * r / 6
        
        lEyeOutline = (-spaceX + offX, offY) : map (\a -> ((-spaceX + offX) + (7 * outlineR / 8) * cos a, offY + outlineR * sin a)) [0, pi/16..2*pi]
        rEyeOutline = map (\a -> ((spaceX + offX) + (7 * outlineR / 8) * cos a, offY + outlineR * sin a)) [0, pi/16..2*pi]
        lEyeOutlineVertices = map (pointToVertex . scaleAndPosition) lEyeOutline
        rEyeOutlineVertices = map (pointToVertex . scaleAndPosition) rEyeOutline
 
        lEyeWhite = map (\a -> ((-spaceX + offX) + (7 * whiteR / 8) * cos a, offY + whiteR * sin a)) [0, pi/16..2*pi]
        rEyeWhite = map (\a -> ((spaceX + offX) + (7 * whiteR / 8) * cos a, offY + whiteR * sin a)) [0, pi/16..2*pi]
        lEyeWhiteVertices = map (pointToVertex . scaleAndPosition) lEyeWhite
        rEyeWhiteVertices = map (pointToVertex . scaleAndPosition) rEyeWhite

        pupilR = r / 6
        pupilOffX = fst eyesPos * r / 4
        pupilOffY = snd eyesPos * r / 5
        lPupil = map (\a -> ((-spaceX + pupilOffX) + pupilR * cos a, offY + pupilOffY + pupilR * sin a)) [0, pi/16..2*pi]
        rPupil = map (\a -> ((spaceX + pupilOffX) + pupilR * cos a, offY + pupilOffY + pupilR * sin a)) [0, pi/16..2*pi]
        lPupilVertices = map (pointToVertex . scaleAndPosition) lPupil
        rPupilVertices = map (pointToVertex . scaleAndPosition) rPupil

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
