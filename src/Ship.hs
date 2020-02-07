{-# LANGUAGE Arrows #-}

module Ship where

import Collision.Util (moveAlongAxis, degToRad)
import Control.Lens
import FRP.Yampa
import Linear
import Physics
import Types
import YampaUtils.Types ()

thrustForce :: (RealFloat a) => a
thrustForce = 3000

shipMovement :: (RealFloat a) => V2 a -> V2 a -> SF (Bool, a) (V2 a, V2 a)
shipMovement initPos initVelocity = proc (thrusterPressed, rot) -> do
  vel <- integralFrom initVelocity -< objectGravity + (if thrusterPressed then moveAlongAxis (V2 0 0) thrustForce (degToRad rot) else 0)
  pos <- integralFrom initPos -< vel
  returnA -< (pos, vel)

shipControl :: (RealFloat a) => Object a -> V2 a -> SF (V2 a) (Object a, V2 a, a)
shipControl initObj initVel = proc inputDir -> do
  let movement2 = (inputDir ^. _y) > 0
  rot <- shipRotationSwitch (realToFrac (initObj ^. rot)) -< realToFrac $ inputDir ^. _x
  -- TODO fix this conversion
  (pos, vel) <- shipMovement (initObj ^. pos) initVel -< (movement2, realToFrac rot)
  returnA -< ((Object pos (initObj ^. size) (realToFrac rot)), vel, realToFrac rot)

shipRotationSwitch :: Double -> SF Double Double
shipRotationSwitch initRot = proc turn -> do
  -- Stop moving or something if a collision is detected
  rot <- integralFrom initRot -< turn * 100
  -- rot <- integralFrom initRot -< rotVel
  returnA -< rot
