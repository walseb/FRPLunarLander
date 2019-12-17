{-# LANGUAGE Arrows #-}

module Physics where

import FRP.BearRiver as B
import FRP.Yampa as Y
import Linear
import YampaUtils.Types ()

objectGravity :: (Fractional a) => V2 a
objectGravity = V2 0 1

objectSpeed :: (Integral a) => a
objectSpeed = 100

movingObject :: (RealFloat a) => V2 a -> Y.SF (V2 a) (V2 a)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  -- TODO: this fromIntegral seems to convert the numbers into Integer which is inefficient
  p <- Y.integralFrom initialPos -< (fromIntegral objectSpeed * move)
  returnA -< p
