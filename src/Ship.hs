{-# LANGUAGE Arrows #-}

module Ship where

import Control.Lens
import FRP.BearRiver as B
import FRP.Yampa as Y
import Input
import Linear
import YampaUtils.Types ()
import Physics

shipMovement :: (RealFloat a) => V2 a -> V2 a -> Y.SF Bool (V2 a, Y.Event (V2 a, V2 a))
shipMovement initialPos initialVelocity = proc input -> do
  v <- Y.integralFrom initialVelocity -< objectGravity
  p <- Y.integralFrom initialPos -< v
  returnA -<
    ( p,
      case input of
        True -> Y.Event (p, v)
        False -> Y.NoEvent
    )

shipThrusters :: (RealFloat a) => V2 a -> V2 a -> Y.SF Bool (V2 a)
shipThrusters initialPos initialVel =
  B.switch
    (Y.iPre False >>> shipMovement initialPos initialVel)
    (\(pos, V2 velX velY) -> shipThrusters pos (V2 velX (velY - 10)))

shipControl :: (RealFloat a) => V2 a -> V2 a -> Y.SF DirectionalInput (a, V2 a)
shipControl initialPos initialVel = proc movement -> do
  let movement2 = movement ^. (up . pressed)
  pos <- shipThrusters initialPos initialVel -< movement2
  rot <- shipRotation 0 -< (movement ^. (Input.left . pressed), movement ^. (Input.right . pressed))
  returnA -< (realToFrac rot, pos)

shipRotation :: Double -> Y.SF (Bool, Bool) Double
shipRotation initialRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  p <- B.integralFrom initialRot -< (boolToInt right - boolToInt left) * 50
  returnA -< p
  where
    boolToInt :: Bool -> Double
    boolToInt a = if a then 0 else 1
