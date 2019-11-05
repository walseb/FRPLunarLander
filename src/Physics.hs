{-# LANGUAGE Arrows #-}

module Physics where

import FRP.BearRiver as B
import FRP.Yampa as Y
import Foreign.C.Types
import Linear
import YampaUtils.Types ()

objectGravity :: V2 Double
objectGravity = V2 0 100

objectSpeed :: V2 Double
objectSpeed = 100

movingObject :: V2 Double -> Y.SF (V2 CInt) (V2 CInt)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  p <- Y.integralFrom initialPos -< objectSpeed * fmap fromIntegral move
  returnA -< fmap floor p
