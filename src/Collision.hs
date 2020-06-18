module Collision where

import Control.Lens
import Control.Monad
import Data.Foldable
import Types
import FRPEngine.Physics.Collision.GJK

import FRPEngine.Types
import FRPEngine.Physics.Collision.Types
import FRPEngine.Physics.Collision.Util

collidesScore :: (Number a) => [[Pt' a]] -> ([[Pt' a]], Int) -> Maybe Int
collidesScore pts (pts', score) =
  case collides' pts pts' of
    True -> Just score
    False -> Nothing

collidesWrapScore :: (Number a) => Scene a -> CollObj a b -> PlayerCollided
collidesWrapScore (Scene terrain landingSpots) player =
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
    playerObj = getCollisionPointsPos player
    playerHitTerrain =
      collides'
        playerObj
        (join (fmap getCollisionPointsPos terrain))
    playerHitLandingspot =
      asum $
        collidesScore
          playerObj
          <$> ( zip
                  (fmap (getCollisionPointsPos . (^. lCollObj)) landingSpots)
                  (fmap (^. pointValue) landingSpots)
              )
