{-# LANGUAGE Arrows #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Physics where

import FRP.Rhine as R
import RhineUtils.Types ()
import Linear

objectGravity :: (Fractional a) => V2 a
objectGravity = V2 0 1

objectSpeed :: (Integral a) => a
objectSpeed = 100

movingObject :: (RealFloat a, a ~ Double, Monad m, TimeDomain td, R.Diff td ~ Double) => V2 a -> BehaviorF m td (V2 a) (V2 a)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  -- TODO: this fromIntegral seems to convert the numbers into Integer which is inefficient
  p <- integralFrom initialPos -< (fromIntegral objectSpeed * move)
  returnA -< p
