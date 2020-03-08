module Collision where

import Control.Lens
import Control.Monad
import Data.Foldable
import Types
import FRPEngine.Collision.Internal.GJK

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
                  (fmap (^. (lCollObj . coll)) landingSpots)
                  (fmap (^. pointValue) landingSpots)
              )

ptsApplyObject :: CollObj SpriteSelect -> CollObj SpriteSelect
ptsApplyObject (CollObj coll obj) =
   CollObj
      ((fmap . fmap) (ptsTransform obj) coll)
      obj
  where
    ptsTransform :: (RealFloat a) => Object a w -> V2 a -> V2 a
    ptsTransform (Object pos size rot _) pt = rotateAroundAxis (degToRad rot) (pos + (size * pt)) pos
