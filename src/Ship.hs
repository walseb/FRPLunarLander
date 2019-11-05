{-# LANGUAGE Arrows #-}

module Ship where

import Control.Lens
import FRP.BearRiver as B
import FRP.Yampa as Y
import Foreign.C.Types
import Input
import Linear
import YampaUtils.Types ()
import Physics

shipMovement :: V2 Double -> V2 Double -> Y.SF Bool (V2 CInt, Y.Event (V2 Double, V2 Double))
shipMovement initialPos initialVelocity = proc input -> do
  v <- Y.integralFrom initialVelocity -< objectGravity
  p <- Y.integralFrom initialPos -< v
  returnA -<
    ( fmap floor p,
      case input of
        True -> Y.Event (p, v)
        False -> Y.NoEvent
    )

shipThrusters :: V2 Double -> V2 Double -> Y.SF Bool (V2 CInt)
shipThrusters initialPos initialVel =
  B.switch
    (Y.iPre False >>> shipMovement initialPos initialVel)
    (\(pos, V2 velX velY) -> shipThrusters pos (V2 velX (velY - 10)))

shipControl :: V2 Double -> V2 Double -> Y.SF MovementVector (Double, V2 CInt)
shipControl initialPos initialVel = proc movement -> do
  let movement2 = movement ^. (up . pressed)
  pos <- shipThrusters initialPos initialVel -< movement2
  rot <- shipRotation 0 -< (movement ^. (Input.left . pressed), movement ^. (Input.right . pressed))
  returnA -< (rot, pos)

shipRotation :: Double -> Y.SF (Bool, Bool) Double
shipRotation initialRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  p <- B.integralFrom initialRot -< ((boolToInt right - boolToInt left) * 50)
  returnA -< p
  where
    boolToInt :: Bool -> Double
    boolToInt a = if a then 0 else 1
