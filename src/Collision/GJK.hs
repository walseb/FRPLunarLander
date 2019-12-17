{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module Collision.GJK
  ( checkCollisions )
where

import Collision.GJKInternal.Support
import qualified Debug.Trace as Tr
-- import FRP.BearRiver as B
-- import FRP.Yampa as Y
import FRP.Rhine
import GJK.Collision
import GJK.Mink (Mink)
import Linear
import Collision.GJKInternal.Util
import qualified Debug.Trace as Tr

-- delay :: (Monad m, TimeDomain td, Diff td ~ Double) => a -> BehaviourF m td a b
-- delay a = proc b -> do
--   _ <- iPre True
--   returnA -< b

-- delay :: (Monad m, TimeDomain td, Diff td ~ Double) => a -> BehaviourF m td a a
-- delay = iPre

myDelay :: (RealFloat a, Monad m, TimeDomain td, Diff td ~ Double) => BehaviourF (ExceptT () m) td ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool
myDelay = proc b -> do
  a <- iPre False -< True
  throwOn () -< a
  returnA -< False

waiting :: (Monad m, TimeDomain td, Diff td ~ Double) => BehaviourF (ExceptT Bool m) td ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool
waiting =  arr (const True)

checkCollisions :: (RealFloat a, Monad m, TimeDomain td, Diff td ~ Double) => BehaviourFExcept m td ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool void
checkCollisions = do
  -- _ <- (delay (((V2 7 7),(V2 7 7),3),((V2 7 7),(V2 7 7),3)) >>> arr (const False))

  -- try myDelay
  todo <- try checkCollisionsEvent
  -- try myDelay
  _ <- try waiting
  checkCollisions

checkCollisionsEvent :: (RealFloat a, Monad m, TimeDomain td, Diff td ~ Double) => BehaviourF (ExceptT () m) td ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool
checkCollisionsEvent = proc (a, b) -> do
  -- throwS True -< True
  throwOn () -< if collides a b == Just True then True else False
  returnA -< False

collides :: (RealFloat a) => (V2 a, V2 a, a) -> (V2 a, V2 a, a) -> Maybe Bool
collides (pos, size, rot) (pos', size', rot') =
  let test = collision 5 a b
   in case test of
        Just True ->
          Tr.trace
            ("Collision 1: " ++ show (toPt pos size rot))
            Tr.trace
            ("Collision 2: " ++ show (toPt pos' size' rot'))
            Tr.trace
            ("Just one last check to collision: " ++ show (collision 5 (toPt pos size rot, polySupport') (toPt pos' size' rot', polySupport')))
            Just
            True
        _ -> test
  where
    a = (toPt pos size rot, polySupport') :: Mink [Pt']
    b = (toPt pos' size' rot', polySupport') :: Mink [Pt']
