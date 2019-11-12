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
    topLeft = moveAlongAxis pos ((size ^. _y) / 2) (rot - (45 + 90))
    topRight = moveAlongAxis pos ((size ^. _y) / 2) (rot - 45)
    botLeft = moveAlongAxis pos ((size ^. _y) / 2) ((45 + 90) + rot)
    botRight = moveAlongAxis pos ((size ^. _y) / 2) (45 + rot)

-- Rot is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist rot =
  V2 x1 y1
  where
    x1 = x + sin (toRad rot) * dist
    y1 = y + cos (toRad rot) * dist
