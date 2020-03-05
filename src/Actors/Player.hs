{-# LANGUAGE Arrows #-}

module Actors.Player (collisionWinSwitch) where

import Collision.Collision
import Control.Lens
import Actors.Enemy (enemyBehavior)
import FRP.Yampa
import Input.Types
import Linear
import Ship (shipControl)
import Types
import Collision.Types
import qualified Debug.Trace as Tr

livingMovementScore :: (RealFloat a) => Player -> V2 a -> [Living] -> Scene -> SF InputState ((Player, [Living]), Event (Maybe (Player, [Living], V2 a, Int)))
livingMovementScore p@(Player (Living _ iPlayerObj) _ initFuel) playerVelInit intiEnemies scene = proc input -> do
  (playerObj, playerVel, playerRot, playerFuel) <- shipControl (iPlayerObj) (fmap realToFrac playerVelInit) initFuel -< vectorizeMovement (input ^. movement)
  enemy <- enemyBehavior (head intiEnemies ^. lObj) -< (V2 0 (-1))
  let player' = ((fuel .~ playerFuel) (((pLiving . lObj) .~ playerObj) p))
  let playerCollision = collidesWrapScore scene (MovingState player' [(Living True enemy)])
  -- returnA -< (Living True playerObj, [(Living True enemy)])
  returnA -<
    ( (player', [(Living True enemy)]),
      case playerCollision of
        NoHit -> NoEvent
        HitUnlandable -> Event Nothing
        (HitLandable score) ->
          case (sum (abs playerVel) < 500) && ((playerRot < 10) || (playerRot > 350)) of
            True -> Event $ Just (player', [(Living True enemy)], fmap realToFrac playerVel, score)
            False -> Event Nothing
    )

vectorizeMovement :: (RealFloat a) => DirectionalInput -> V2 a
vectorizeMovement
  ( DirectionalInput
      (ButtonState _ a0)
      (ButtonState _ a1)
      (ButtonState _ a2)
      (ButtonState _ a3)
    ) =
    V2
      -- Horizontal
      (boolToInt a2 - boolToInt a3)
      -- Vertical
      (boolToInt a1 - boolToInt a0)
    where
      boolToInt :: (RealFloat a) => Bool -> a
      boolToInt a = if a then 0 else 1
vectorizeMovement _ = error "Trying to vectorize unsupported input"

-- Same as collisionSwitch except instead of keeping the player from falling through object on proper non-lethal collision with landing spot it awards the player with points
collisionWinSwitch :: (RealFloat a) => Player -> [Living] -> Scene -> V2 a -> SF InputState (Player, [Living])
collisionWinSwitch player enemies scene playerInitVel =
  switch
    (livingMovementScore player playerInitVel enemies scene)
    ( \d ->
        case d of
          (Just (player', enemies', playerVel, score)) -> collisionWinSwitch (nextLevel player' score) enemies scene playerInitVel
          Nothing -> constant ((((pLiving . alive) .~ False) player), enemies)
    )
  where
    nextLevel = addScore . ((pLiving . lObj . pos) .~ (V2 100 100)) . (fuel %~ (+3))
    addScore :: Player -> Int -> Player
    addScore player' score' = (score `over` (+ score')) player'
