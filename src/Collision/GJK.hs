{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Collision.GJK
  ( collidesWrap,
    collidesWrapScore,
    collides',
    collidesScore,
  )
where

import Collision.GJKInternal.Support
import Collision.GJKInternal.Util
import Control.Applicative
import Control.Lens
import Control.Monad
import GJK.Collision
import Types

type PlayerHitLandingspot = Bool

collidesScore :: (RealFloat a) => [[Pt' a]] -> ([[Pt' a]], Int) -> Maybe Int
collidesScore pts (pts', score) =
  case collides' pts pts' of
    True -> Just score
    False -> Nothing

collidesWrapScore :: Scene -> MovingState -> Maybe (PlayerHitLandingspot, Int)
collidesWrapScore (Scene terrain landingSpots) (MovingState player enemies) =
  case playerHitTerrain of
    True ->
      Just (False, 0)
    False ->
      case playerHitLandingspot of
        Just score ->
          Just (True, score)
        Nothing ->
          Nothing
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
