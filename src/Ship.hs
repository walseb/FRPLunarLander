{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Ship where

import Collision.GJKInternal.Util (moveAlongAxis)
import Control.Lens
import FRP.BearRiver as B
import FRP.Yampa as Y
import Input
import Linear
import Physics
import YampaUtils.Types ()
import qualified Debug.Trace as Tr

-- Return: (V2 a, Y.Event (V2 a, V2 a)) where the first V2 is gravity, and the event contains a position
shipMovement :: (RealFloat a) => V2 a -> V2 a -> V2 a -> Y.SF (Bool, a) (V2 a, Y.Event (V2 a, V2 a, V2 a))
shipMovement initPos initVelocity initForce = proc (input, rot) -> do
  force <- Y.integralFrom initForce -< objectGravity
  vel <- Y.integralFrom initVelocity -< force
  pos <- Y.integralFrom initPos -< vel
  returnA -<
    ( pos,
      case input of
        True -> Y.Event (pos, vel, force + moveAlongAxis (V2 0 0) 3 (rot + 90))
        False -> Y.NoEvent
    )

shipMovementSwitch :: (RealFloat a) => V2 a -> V2 a -> V2 a -> Y.SF (Bool, a) (V2 a)
shipMovementSwitch initPos initVel initForce =
  B.switch
    -- TODO the reason why the ship goes faster whenever you hit up is because of the iPre here, it runs the SF faster because it lets the function run at least twice
    (Y.iPre (False, 0) >>> shipMovement initPos initVel initForce)
    (\ (a, b, c) -> shipMovementSwitch a b c)

shipControl :: (RealFloat a) => V2 a -> V2 a -> Y.SF DirectionalInput (a, V2 a)
shipControl initPos initVel = proc inputDir -> do
  let movement2 = inputDir ^. (up . pressed)
  rot <- shipRotationSwitch 0 -< (inputDir ^. (Input.left . pressed), inputDir ^. (Input.right . pressed))
  -- TODO fix this conversion
  pos <- shipMovementSwitch initPos initVel 0 -< (movement2, realToFrac rot)
  returnA -< (realToFrac rot, pos)


-- shipRotationSwitch :: Double -> Y.SF (Bool, Bool) Double
-- shipRotationSwitch initRot =
--   B.switch
--     (shipRotation initRot)
--     shipRotationSwitch

-- shipRotation :: Double -> Y.SF (Bool, Bool) (Double, Y.Event Double)
-- shipRotation initRot = proc (left, right) -> do
--   -- Stop moving or something if a collision is detected
--   rotVel <- B.integralFrom initRot -< (boolToInt right - boolToInt left) * 50
--   rot <- B.integralFrom initRot -< rotVel
--   returnA -< (rot,
--              case rot > 360 of
--                True -> Y.Event (rot - 360)
--                False ->
--                  case rot < 0 of
--                    True -> Y.Event (rot + 360)
--                    False -> Y.NoEvent)
--   where
--     boolToInt :: Bool -> Double
--     boolToInt a = if a then 0 else 1

shipRotationSwitch :: Double -> Y.SF (Bool, Bool) Double
shipRotationSwitch initRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  rot <- B.integralFrom initRot -< (boolToInt right - boolToInt left) * 100
  -- rot <- B.integralFrom initRot -< rotVel
  returnA -< rot
  where
    boolToInt :: Bool -> Double
    boolToInt a = if a then 0 else 1
