{-# LANGUAGE Arrows #-}

module Enemy where

import Linear
import FRP.Yampa
import YampaUtils.Types ()

movementSpeed :: (RealFloat a) => V2 a
movementSpeed = V2 0 300

enemyMovement :: (RealFloat a) => V2 a -> SF (V2 a) (V2 a)
enemyMovement initialPos = proc move -> do
  -- TODO: this fromIntegral seems to convert the numbers into Integer which is inefficient
  p <- integralFrom initialPos -< movementSpeed * move
  returnA -< p
