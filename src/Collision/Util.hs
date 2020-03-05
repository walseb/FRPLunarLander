{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision.Util (moveAlongAxis, objToRect, ptsApplyObject, degToRad) where

import Control.Lens
import Linear
import Types
import Collision.Types

ptsApplyObject :: Terrain -> Terrain
ptsApplyObject (Terrain coll obj) =
   Terrain
      ((fmap . fmap) (ptsTransform obj) coll)
      obj
  where
    ptsTransform :: (RealFloat a) => Object a -> V2 a -> V2 a
    ptsTransform (Object pos size rot _) pt = rotateAroundAxis (degToRad rot) (pos + (size * pt)) pos

-- Takes rad as rot
rotateAroundAxis :: (RealFloat a) => a -> V2 a -> V2 a -> V2 a
rotateAroundAxis theta (V2 x y) (V2 xO yO) =
  V2 x' y'
  where
    x' = xO + (x - xO) * cos theta - (y - yO) * sin theta
    y' = yO + (x - xO) * sin theta + (y - yO) * cos theta

objToRect :: (RealFloat a) => Object a -> [Pt' a]
-- objToRect pos size rot =
objToRect obj =
  [topLeft, topRight, botLeft, botRight]
  where
    pos' = obj ^. pos
    size' = obj ^. size
    rot' = degToRad (obj ^. rot)
    -- TODO: Doesn't work on rectangles
    midRightLocal = moveAlongAxis pos' ((size' ^. _x) / 2) rot' - pos'
    midBotLocal = moveAlongAxis pos' ((size' ^. _y) / 2) (rot' - degToRad 90) - pos'
    topRight = pos' + midRightLocal - midBotLocal
    topLeft = (pos' - midRightLocal) - midBotLocal
    botLeft = (pos' - midRightLocal) + midBotLocal
    botRight = pos' + midRightLocal + midBotLocal

degToRad :: (Floating a) => a -> a
degToRad theta = theta / 180 * pi

-- Rot is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist theta =
  V2 x1 y1
  where
    x1 = x + sin theta * dist
    y1 = y + cos theta * dist