module Collision.Collision where

import Collision.Internal.GJK
import Collision.Types
import Collision.Util
import Control.Lens
import Control.Monad
import Data.Foldable
import Types

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
    playerObj = [objToRect (player ^. (pLiving . lObj))]
    playerHitTerrain =
      collides'
        playerObj
        ( [((objToRect (enemies ^. (to head . lObj))))]
            ++ [((objToRect (enemies ^. (to head . lObj))))]
            ++ (join (fmap (^. coll) terrain))
        )
    playerHitLandingspot =
      asum $
        collidesScore
          playerObj
          <$> ( zip
                  (fmap (^. (lTerrain . coll)) landingSpots)
                  (fmap (^. pointValue) landingSpots)
              )
