{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module Ship where

import Collision.GJKInternal.Util (moveAlongAxis)
import Control.Lens
import Input
import Linear
import Physics
import RhineUtils.Types ()
import FRP.Rhine
import qualified Debug.Trace as Tr

-- Return: (V2 a, Y.Event (V2 a, V2 a)) where the first V2 is gravity, and the event contains a position

-- Y.Event (V2 a, V2 a, V2 a)
-- shipMovement :: (RealFloat a, a ~ Double, Monad m, TimeDomain td, Diff td ~ Double) => V2 a -> V2 a -> V2 a -> BehaviourF (ExceptT (V2 a, V2 a, V2 a) m) td (Bool, a) (V2 a)
-- shipMovement initPos initVelocity initForce = proc (input, rot) -> do
--   force <- integralFrom initForce -< objectGravity
--   vel <- integralFrom initVelocity -< force
--   pos <- integralFrom initPos -< vel
--   throwMaybe -< if (input == True) then Just (pos, vel, force + moveAlongAxis (V2 0 0) 3 (rot + 90)) else Nothing
--   returnA -< pos

myDelay :: (RealFloat a, Monad m, TimeDomain td, Diff td ~ Double) => V2 a -> BehaviourF (ExceptT () m) td (Bool, a) (V2 a)
myDelay c = proc b -> do
  a <- iPre False -< True
  throwOn () -< a
  returnA -< c

shipMovement :: (RealFloat a, a ~ Double, Monad m, TimeDomain td, Diff td ~ Double) => V2 a -> V2 a -> V2 a -> BehaviourF (ExceptT (V2 a, V2 a, V2 a) m) td (Bool, a) (V2 a)
shipMovement initPos initVelocity initForce = proc (input, rot) -> do
  force <- integralFrom initForce -< objectGravity
  vel <- integralFrom initVelocity -< force
  pos <- integralFrom initPos -< vel
  throwMaybe -< if input then Just (pos, vel, force + moveAlongAxis (V2 0 0) 10 (rot + 90)) else Nothing
  returnA -< pos

shipMovementSwitch :: (RealFloat a, a ~ Double, Monad m, TimeDomain td, Diff td ~ Double) => V2 a -> V2 a -> V2 a -> BehaviourFExcept m td (Bool, a) (V2 a) void
shipMovementSwitch initPos initVel initForce = do
  try (myDelay initPos)
  (pos, vel, force) <- try $ shipMovement initPos initVel initForce
  shipMovementSwitch pos vel force

shipControl :: (RealFloat a, a ~ Double, Monad m, TimeDomain td, Diff td ~ Double) => V2 a -> V2 a -> BehaviourF m td DirectionalInput (a, V2 a)
shipControl initPos initVel = proc inputDir -> do
  let movement2 = inputDir ^. (up . pressed)
  rot <- shipRotationSwitch 0 -< (inputDir ^. (Input.left . pressed), inputDir ^. (Input.right . pressed))
  -- TODO fix this conversion
  pos <- safely $ shipMovementSwitch initPos initVel 0 -< (movement2, realToFrac rot)
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

shipRotationSwitch :: (Monad m, TimeDomain td, Diff td ~ Double) => Double -> BehaviourF m td (Bool, Bool) Double
shipRotationSwitch initRot = proc (left, right) -> do
  -- Stop moving or something if a collision is detected
  rot <- integralFrom initRot -< (boolToInt right - boolToInt left) * 100
  -- rot <- B.integralFrom initRot -< rotVel
  returnA -< rot
  where
    boolToInt :: Bool -> Double
    boolToInt a = if a then 0 else 1
