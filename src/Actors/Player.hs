{-# LANGUAGE Arrows #-}

module Actors.Player
  ( collisionWinSwitch,
  )
where

import Collision
import Control.Lens
import FRP.Yampa
import FRPEngine.Collision.Types
import FRPEngine.Input.Interpreter
import FRPEngine.Input.Types
import FRPEngine.Types
import Linear
import Ship (shipControl)
import Types

livingMovementScore :: (RealFloat a) => Player -> V2 a -> Scene -> SF InputState ((Player), Event (Maybe (Player, V2 a, Int)))
livingMovementScore p@(Player (Living _ iPlayerObj) _ initFuel) playerVelInit scene = proc input -> do
  (playerObj, playerVel, playerRot, playerFuel) <- shipControl (iPlayerObj) (fmap realToFrac playerVelInit) initFuel -< vectorizeMovement (input ^. movement)
  let player' = ((fuel .~ playerFuel) (((pLiving . lObj) .~ playerObj) p))
  let playerCollision = collidesWrapScore scene (MovingState player')
  returnA -<
    ( (player'),
      case playerCollision of
        NoHit -> NoEvent
        HitUnlandable -> Event Nothing
        (HitLandable score) ->
          case (sum (abs playerVel) < 500) && ((playerRot < 10) || (playerRot > 350)) of
            True -> Event $ Just (player', fmap realToFrac playerVel, score)
            False -> Event Nothing
    )

-- Same as collisionSwitch except instead of keeping the player from falling through object on proper non-lethal collision with landing spot it awards the player with points
collisionWinSwitch :: (RealFloat a) => Player -> Scene -> V2 a -> SF InputState (Player)
collisionWinSwitch player scene playerInitVel =
  switch
    (livingMovementScore player playerInitVel scene)
    ( \d ->
        case d of
          (Just (player', playerVel, score)) -> collisionWinSwitch (nextLevel player' score) scene playerInitVel
          Nothing -> constant ((((pLiving . alive) .~ False) player))
    )
  where
    nextLevel =
      addScore
        -- Reset to original position
        . ((pLiving . lObj . pos) .~ (player ^. (pLiving . lObj . pos)))
        -- Add fuel
        . (fuel %~ (+ 2))
    addScore :: Player -> Int -> Player
    addScore player' score' = (score `over` (+ score')) player'
