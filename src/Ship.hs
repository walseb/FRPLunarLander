{-# LANGUAGE Arrows #-}

module Ship where

import Collision.GJKInternal.Util (moveAlongAxis)
import Control.Lens
import FRP.Yampa
import Input
import Linear
import Physics
import YampaUtils.Types ()

thrustForce :: (RealFloat a) => a
thrustForce = 3000

shipMovement :: (RealFloat a) => V2 a -> V2 a -> SF (Bool, a) (V2 a)
shipMovement initPos initVelocity = proc (thrusterPressed, rot) -> do
  vel <- integralFrom initVelocity -< objectGravity + (if thrusterPressed then moveAlongAxis (V2 0 0) thrustForce rot else 0)
  pos <- integralFrom initPos -< vel
  returnA -< pos

shipControl :: (RealFloat a) => V2 a -> V2 a -> SF DirectionalInput (a, V2 a)
shipControl initPos initVel = proc inputDir -> do
  let movement2 = inputDir ^. (up . pressed)
  rot <- shipRotationSwitch 0 -< (inputDir ^. (Input.left . pressed), inputDir ^. (Input.right . pressed))
  -- TODO fix this conversion
  pos <- shipMovement initPos initVel -< (movement2, realToFrac rot)
  returnA -< (realToFrac rot, pos)

shipRotationSwitch :: Double -> SF (Bool, Bool) Double
shipRotationSwitch initRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  rot <- integralFrom initRot -< (boolToInt right - boolToInt left) * 100
  -- rot <- integralFrom initRot -< rotVel
  returnA -< rot
  where
    boolToInt :: Bool -> Double
    boolToInt a = if a then 0 else 1
