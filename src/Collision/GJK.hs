{-# LANGUAGE Arrows #-}

module Collision.GJK
  ( checkCollisions,
  )
where

import Collision.GJKInternal.Support
import Collision.GJKInternal.Util
import qualified Debug.Trace as Tr
import FRP.Yampa
import GJK.Collision
import GJK.Mink (Mink)
import Linear

checkCollisions :: (RealFloat a) => SF ((V2 a, V2 a, a), (V2 a, V2 a, a)) Bool
checkCollisions =
  switch
    checkCollisionsEvent
    ( \a ->
        Tr.trace
          ("Player dead!!: Here: " ++ show a)
          constant
          False
    )

checkCollisionsEvent :: (RealFloat a) => SF ((V2 a, V2 a, a), (V2 a, V2 a, a)) (Bool, Event ())
checkCollisionsEvent = proc (a, b) ->
  returnA -< case collides a b of
    Just True -> (False, Event ())
    Just False -> (True, NoEvent)
    Nothing -> (True, NoEvent)

collides :: (RealFloat a) => (V2 a, V2 a, a) -> (V2 a, V2 a, a) -> Maybe Bool
collides (pos, size, rot) (pos', size', rot') =
  let test = collision 5 a b
   in case test of
        Just True ->
          Tr.trace
            ("Collision at object with position:\n" ++ show (toPt pos size rot) ++ "\nand:\n" ++ show (toPt pos' size' rot'))
            Just
            True
        _ -> test
  where
    a = (toPt pos size rot, polySupport') :: Mink [Pt']
    b = (toPt pos' size' rot', polySupport') :: Mink [Pt']
