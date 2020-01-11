{-# LANGUAGE Arrows #-}

module Collision.GJK
  ( checkCollisions,
  )
where

import Collision.GJKInternal.Support
import qualified Debug.Trace as Tr
import FRP.Yampa
import GJK.Collision
import Control.Applicative

checkCollisions :: (RealFloat a, Show a) => SF ([[Pt' a]], [[Pt' a]]) Bool
checkCollisions =
  switch
    checkCollisionsEvent
    ( \a ->
        Tr.trace
          ("Player dead!!: Here: " ++ show a)
          constant
          False
    )

checkCollisionsEvent :: (RealFloat a) => SF ([[Pt' a]], [[Pt' a]]) (Bool, Event ([[Pt' a]], [[Pt' a]]))
checkCollisionsEvent = proc (a, b) ->
  returnA -< case collides a b of
    True -> (False, Event (a, b))
    False -> (True, NoEvent)

collides :: (RealFloat a) => [[Pt' a]] -> [[Pt' a]] -> Bool
collides pts pts' =
  mapThing $ liftA2 collision' pts pts'
  where
    mapThing :: [Maybe Bool] -> Bool
    mapThing (Just True:_) = True
    mapThing (_:as) = mapThing as
    mapThing [] = False

collision' :: (RealFloat a) => [Pt' a] -> [Pt' a] -> Maybe Bool
collision' pts pts' = collision 5 (pts, polySupport') (pts', polySupport')
