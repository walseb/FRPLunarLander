{-# LANGUAGE LambdaCase #-}

module Collision.Internal.GJK where

import Collision.Internal.GJKInternal.Support
import Control.Applicative
import GJK.Collision
import Collision.Types

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
