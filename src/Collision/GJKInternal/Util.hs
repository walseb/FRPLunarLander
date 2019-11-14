{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Collision.GJKInternal.Util where

import Linear
import Control.Lens
import Collision.GJKInternal.Support

toRad :: (Floating a) => a -> a
toRad a = a / 180 * pi

-- This model doesn't have
--      (x, y) -> +-----+ <- (x + x', y)
--                |     |
--                |     |
-- (x, y + y') -> +-----+ <- (x + x', y + y')
toPt :: (RealFloat a) => V2 a -> V2 a -> a -> [Pt']
toPt pos size rot =
  (fmap . fmap)
    realToFrac
    [ topLeft,
      topRight,
      botLeft,
      botRight
    ]
  where
    -- TODO: Doesn't work on rectangles
    midRightLocal = moveAlongAxis pos ((size ^. _x) / 2) rot - pos
    midBotLocal = moveAlongAxis pos ((size ^. _y) / 2) (rot - 90) - pos
    topRight = pos + midRightLocal - midBotLocal
    topLeft = (pos - midRightLocal) - midBotLocal
    botLeft = (pos - midRightLocal) + midBotLocal
    botRight = pos + midRightLocal + midBotLocal

-- Rot is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist rot =
  V2 x1 y1
  where
    x1 = x + sin (toRad rot) * dist
    y1 = y + cos (toRad rot) * dist
