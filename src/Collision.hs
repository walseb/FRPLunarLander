module Collision where

import Control.Lens
import Control.Monad
import Data.Foldable
import Types
import FRPEngine.Collision.GJK

import Linear
import FRPEngine.Types
import FRPEngine.Collision.Types
import FRPEngine.Collision.Util

collidesScore :: (RealFloat a) => [[Pt' a]] -> ([[Pt' a]], Int) -> Maybe Int
collidesScore pts (pts', score) =
  case collides' pts pts' of
    True -> Just score
    False -> Nothing

collidesWrapScore :: Scene -> MovingState -> PlayerCollided
collidesWrapScore (Scene terrain landingSpots) (MovingState player) =
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
    playerObj = getCollisionPointsPos (player ^. (pLiving . liCollObj))
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
