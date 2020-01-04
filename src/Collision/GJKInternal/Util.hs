{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision.GJKInternal.Util where

import Collision.GJKInternal.Support
import Control.Lens
import Linear
import Types

ptsApplyObject :: Object -> [[Pt']] -> [[Pt']]
ptsApplyObject obj pts =
  (fmap . fmap) (ptsTransform obj') pts
  where
    obj' = (obj ^. pos, obj ^. size, obj ^. rot)

ptsTransform :: (RealFloat a) => (V2 a, V2 a, a) -> V2 a -> V2 a
ptsTransform (pos, size, rot) pt = rotateAroundAxis rot (size * (pos + pt)) pos

rotateAroundAxis :: (RealFloat a) => a -> V2 a -> V2 a -> V2 a
rotateAroundAxis theta (V2 x y) (V2 xO yO)  =
  V2 x' y'
  where
    x' = xO + (x - xO) * cos(theta) - (y - yO) * sin(theta)
    y' = yO + (x - xO) * sin(theta) + (y - yO) * cos(theta)

toPt :: (RealFloat a) => V2 a -> V2 a -> a -> [Pt']
toPt pos size rot =
  (fmap . fmap) realToFrac [topLeft, topRight, botLeft, botRight]
  where
    -- TODO: Doesn't work on rectangles
    midRightLocal = moveAlongAxis pos ((size ^. _x) / 2) (rot + 90) - pos
    midBotLocal = moveAlongAxis pos ((size ^. _y) / 2) rot - pos
    topRight = pos + midRightLocal - midBotLocal
    topLeft = (pos - midRightLocal) - midBotLocal
    botLeft = (pos - midRightLocal) + midBotLocal
    botRight = pos + midRightLocal + midBotLocal

degToRad :: (Floating a) => a -> a
degToRad theta = theta / 180 * pi

-- Rot is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist rotRight =
  V2 x1 y1
  where
    rotUp = rotRight + 90
    x1 = x + sin (degToRad rotUp) * dist
    y1 = y + cos (degToRad rotUp) * dist
