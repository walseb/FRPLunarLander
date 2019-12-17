{-# LANGUAGE Arrows #-}

module Collision.GJK
  ( checkCollisions )
where

import Collision.GJKInternal.Support
import qualified Debug.Trace as Tr
import FRP.BearRiver as B
import FRP.Yampa as Y
import GJK.Collision
import GJK.Mink (Mink)
import Linear
import Collision.GJKInternal.Util

checkCollisions :: (RealFloat a) => Y.SF ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool
checkCollisions =
  B.switch
    checkCollisionsEvent
    ( \a ->
        Tr.trace
          ("Player dead!!: Here: " ++ show a)
          constant
          False
    )

checkCollisionsEvent :: (RealFloat a) => Y.SF ((V2 a, V2 a, a), (V2 a, V2 a, a)) (Bool, Y.Event ())
checkCollisionsEvent = proc (a, b) ->
  returnA -< case collides a b of
    Just True -> (False, Y.Event ())
    Just False -> (True, Y.NoEvent)
    Nothing -> (True, Y.NoEvent)

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
