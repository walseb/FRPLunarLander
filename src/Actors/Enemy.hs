{-# LANGUAGE Arrows #-}

module Actors.Enemy (enemyBehavior) where

import Linear
import FRP.Yampa
import FRPEngine.YampaUtils.Types ()
import Control.Lens
import FRPEngine.Types

movementSpeed :: (RealFloat a) => V2 a
movementSpeed = V2 0 300

enemyMovement :: (RealFloat a) => V2 a -> SF (V2 a) (V2 a)
enemyMovement initialPos = proc moveDir ->
  integralFrom initialPos -< movementSpeed * moveDir

enemyBehavior :: (RealFloat a) => Object a w -> SF (V2 a) (Object a w)
enemyBehavior initObj = proc moveDir -> do
  -- TODO: this fromIntegral seems to convert the numbers into Integer which is inefficient
  a <- (enemyMovement (initObj ^. pos)) -< moveDir
  returnA -< pos .~ a $ initObj
