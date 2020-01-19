{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision.GJKInternal.Util where

import Collision.GJKInternal.Support
import Control.Lens
import Linear
import Types

ptsApplyObject :: (RealFloat a) => Object a -> [[Pt' a]] -> [[Pt' a]]
ptsApplyObject obj pts =
  (fmap . fmap) (ptsTransform obj) pts

ptsTransform :: (RealFloat a) => Object a -> V2 a -> V2 a
ptsTransform (Object pos size rot) pt = rotateAroundAxis (degToRad rot) (pos + (size * pt)) pos

-- Takes rad as rot
rotateAroundAxis :: (RealFloat a) => a -> V2 a -> V2 a -> V2 a
rotateAroundAxis theta (V2 x y) (V2 xO yO)  =
  V2 x' y'
  where
    x' = xO + (x - xO) * cos(theta) - (y - yO) * sin(theta)
    y' = yO + (x - xO) * sin(theta) + (y - yO) * cos(theta)

toPt :: (RealFloat a) => (Object a) -> [Pt' a]
-- toPt pos size rot =
toPt obj =
  [topLeft, topRight, botLeft, botRight]
  where
    rotRad = degToRad (obj ^. rot)
    -- TODO: Doesn't work on rectangles
    midRightLocal = moveAlongAxis (obj ^. pos) (((obj ^. size) ^. _x) / 2) rotRad - (obj ^. pos)
    midBotLocal = moveAlongAxis (obj ^. pos) (((obj ^. size) ^. _y) / 2) (rotRad + (degToRad 90)) - (obj ^. pos)
    topRight = (obj ^. pos) + midRightLocal - midBotLocal
    topLeft = ((obj ^. pos) - midRightLocal) - midBotLocal
    botLeft = ((obj ^. pos) - midRightLocal) + midBotLocal
    botRight = (obj ^. pos) + midRightLocal + midBotLocal

degToRad :: (Floating a) => a -> a
degToRad theta = theta / 180 * pi

-- Rot is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist theta =
  V2 x1 y1
  where
    x1 = x + sin theta * dist
    y1 = y + cos theta * dist
