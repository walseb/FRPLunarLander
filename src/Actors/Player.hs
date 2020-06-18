{-# LANGUAGE Arrows #-}

module Actors.Player
  ( collisionWinSwitch,
  )
where

import Collision
import Control.Lens
import FRP.Yampa
import FRPEngine.Physics.Collision.Types
import FRPEngine.Input.Utils
import FRPEngine.Input.Types
import FRPEngine.Types
import Linear
import Ship (shipControl)
import Types
import Input

livingMovementScore :: (Number a) => Player a -> V2 a -> Scene a -> SF [Input] ((Player a), Event (Maybe (Player a, V2 a, Int)))
livingMovementScore p@(Player _ iPlayerObj _ initFuel) playerVelInit scene = proc input -> do
  (playerObj, playerVel, playerRot, playerFuel) <- shipControl (iPlayerObj ^. obj) (fmap realToFrac playerVelInit) initFuel -< moveKey input
  let player' = ((fuel .~ playerFuel) (((pCollObj . obj) .~ playerObj) p))
  let playerCollision = collidesWrapScore scene (player' ^. pCollObj)
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

collisionWinSwitch :: (Number a) => Player a -> Scene a -> V2 a -> SF [Input] (Player a)
collisionWinSwitch player scene playerInitVel =
  switch
    (livingMovementScore player playerInitVel scene)
    ( \d ->
        case d of
          (Just (player', playerVel, score)) -> collisionWinSwitch (nextLevel player' score) scene playerInitVel
          Nothing -> constant (((alive .~ False) player))
    )
  where
    nextLevel =
      addScore
        -- Reset to original position
        . ((pCollObj . obj . pos) .~ (player ^. (pCollObj . obj . pos)))
        -- Add fuel
        . (fuel %~ (+ 2))
    addScore :: (Number a) =>  Player a -> Int -> Player a
    addScore player' score' = (score `over` (+ score')) player'
