{-# LANGUAGE Arrows #-}

module Ship where

import FRPEngine.Physics.Collision.Util (moveAlongAxis, degToRad)
import Control.Lens
import FRP.Yampa
import Linear
import FRPEngine.Yampa.Types ()
import FRPEngine.Types

thrustForce :: (Number a) => a
thrustForce = 3000

rotationSpeed :: (Number a) => a
rotationSpeed = 100

gravity :: (Number a) => V2 a
gravity = V2 0 (-300)

shipControl :: (Number a) => Obj a w -> V2 a -> a -> SF (V2 a) (Obj a w, V2 a, a, a)
shipControl initObj initVel initFuel = proc inputDir -> do
  let movement2 = (inputDir ^. _y) > 0
  rot <- rotationSF (realToFrac (initObj ^. rot)) -< inputDir ^. _x
  -- TODO fix this conversion
  (pos, vel, fuel) <- novement (initObj ^. pos) initVel initFuel -< (movement2, rot)
  returnA -< ((Obj pos vel rot (initObj ^. size) (initObj ^. spr) True), vel, rot, fuel)
  where
    rotationSF :: (Number a) => a -> SF a a
    rotationSF initRot = proc theta -> do
      (V1 rot) <- integralFrom (V1 initRot) -< V1 $ theta * rotationSpeed
      returnA -< rot

    novement :: (Number a) => V2 a -> V2 a -> a -> SF (Bool, a) (V2 a, V2 a, a)
    novement initPos initVelocity initFuel = proc (thrusterPressed, rot) -> do
      (V1 fuel) <- integralFrom (V1 initFuel) -< V1 (if thrusterPressed then -1 else 0)
      vel <- integralFrom initVelocity -< gravity + (if (thrusterPressed && fuel > 0) then moveAlongAxis (V2 0 0) thrustForce (degToRad rot) else 0)
      pos <- integralFrom initPos -< vel
      returnA -< (pos, vel, fuel)
