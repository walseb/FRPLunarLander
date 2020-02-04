{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Collision.GJK where

import Collision.GJKInternal.Support
import Collision.GJKInternal.Util
import Control.Applicative
import Control.Lens
import Control.Monad
import GJK.Collision
import Types
import Linear

-- Completely solves collision by moving the player up and re-trying collision test
solveCollision :: Scene -> MovingState -> Player
solveCollision scene (MovingState player enemies) =
  case collidesWrap scene (MovingState player enemies) of
    Just True -> solveCollision scene (MovingState (((pLiving . lObject . pos . _y) `over` (+ 8)) player) enemies)
    Just False -> (Player (Living False (player ^. pLiving . lObject)) (player ^. score))
    Nothing -> player

collidesScore :: (RealFloat a) => [[Pt' a]] -> ([[Pt' a]], Int) -> Maybe Int
collidesScore pts (pts', score) =
  case collides' pts pts' of
    True -> Just score
    False -> Nothing

type Score = Int
data PlayerCollided = HitUnlandable | HitLandable Score | NoHit

collidesWrapScore :: Scene -> MovingState -> PlayerCollided
collidesWrapScore (Scene terrain landingSpots) (MovingState player enemies) =
  case playerHitTerrain of
    True ->
      HitUnlandable
    False ->
      case playerHitLandingspot of
        Just score ->
          HitLandable score
        Nothing ->
          NoHit
  where
    playerObj = [toPt (player ^. (pLiving . lObject))]
    playerHitTerrain =
      collides'
        playerObj
        ( [((toPt (enemies ^. (to head . lObject))))]
            ++ [((toPt (enemies ^. (to head . lObject))))]
            ++ (join (fmap (^. coll) terrain))
        )
    playerHitLandingspot =
      collidesScore
        playerObj
        ( head
            ( zip
                (fmap (^. (lTerrain . coll)) landingSpots)
                (fmap (^. pointValue) landingSpots)
            )
        )

type PlayerHitLandingspot = Bool

-- If nothing then player didn't hit anything. If Just False then player hit the terrain. If Just True then player hit landing spot
collidesWrap :: Scene -> MovingState -> Maybe PlayerHitLandingspot
collidesWrap (Scene terrain landingSpots) (MovingState player enemies) =
  case playerHitTerrain of
    True ->
      Just False
    False ->
      case playerHitLandingspot of
        True ->
          Just True
        False ->
          Nothing
  where
    playerObj = [toPt (player ^. (pLiving . lObject))]
    playerHitTerrain =
      collides'
        playerObj
        ([((toPt (enemies ^. (to head . lObject))))] ++ ((^. (coll)) =<< terrain))
    playerHitLandingspot =
      collides'
        playerObj
        ([((toPt (enemies ^. (to head . lObject))))] ++ ((^. (lTerrain . coll)) =<< landingSpots))

collides' :: (RealFloat a) => [[Pt' a]] -> [[Pt' a]] -> Bool
collides' pts pts' =
  any
    ( \case
        Just True -> True
        _ -> False
    )
    $ liftA2 collides pts pts'

collides :: (RealFloat a) => [Pt' a] -> [Pt' a] -> Maybe Bool
collides pts pts' = collision 5 (pts, polySupport') (pts', polySupport')
