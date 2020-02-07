module Collision.Collision where

import Collision.Util
import Control.Lens
import Control.Monad
import Types
import Collision.Types
import Collision.Internal.GJK

collidesScore :: (RealFloat a) => [[Pt' a]] -> ([[Pt' a]], Int) -> Maybe Int
collidesScore pts (pts', score) =
  case collides' pts pts' of
    True -> Just score
    False -> Nothing

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
