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

checkCollisions :: SF ([[Pt']], [[Pt']]) Bool
checkCollisions =
  switch
    checkCollisionsEvent
    ( \a ->
        Tr.trace
          ("Player dead!!: Here: " ++ show a)
          constant
          False
    )

checkCollisionsEvent :: SF ([[Pt']], [[Pt']]) (Bool, Event ())
checkCollisionsEvent = proc (a, b) ->
  returnA -< case collides a b of
    True -> (False, Event ())
    False -> (True, NoEvent)

collides :: [[Pt']] -> [[Pt']] -> Bool
collides pts pts' =
  mapThing $ liftA2 collision' pts pts'
  where
    mapThing :: [Maybe Bool] -> Bool
    mapThing (Just True:_) = True
    mapThing (_:as) = mapThing as
    mapThing [] = False

collision' :: [Pt'] -> [Pt'] -> Maybe Bool
collision' pts pts' = collision 5 (pts, polySupport') (pts', polySupport')
