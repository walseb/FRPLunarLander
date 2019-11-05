{-# LANGUAGE Arrows #-}

module Collision.GJK
  ( Size (..),
    checkCollisions,
    debugRenderHitbox,
  )
where

import qualified Debug.Trace as Tr
import FRP.BearRiver as B
import FRP.Yampa as Y
import Foreign.C.Types
import GJK.Collision
import GJK.Point
import GJK.Support
import Linear
import qualified SDL.Vect as SV
import qualified Sprite as SP
import Types (Size (..))

checkCollisions :: Y.SF ((V2 CInt, Size, Double), (V2 CInt, Size, Double)) Bool
checkCollisions =
  B.switch
    checkCollisionsEvent
    ( \a ->
        Tr.trace
          ("Player dead!!: Here: " ++ show a)
          constant
          False
    )

checkCollisionsEvent :: Y.SF ((V2 CInt, Size, Double), (V2 CInt, Size, Double)) (Bool, Y.Event ())
checkCollisionsEvent = proc (a, b) ->
  returnA -< case collides a b of
    Just True -> (False, Y.Event ())
    Just False -> (True, Y.NoEvent)
    Nothing -> (True, Y.NoEvent)

v2ToTuple :: V2 a -> (a, a)
v2ToTuple (V2 x y) = (x, y)

tupleToV2 :: (a, a) -> V2 a
tupleToV2 (x, y) = V2 x y

toRad :: (Floating a) => a -> a
toRad a = a / 180 * pi

-- This model doesn't have
--      (x, y) -> +-----+ <- (x + x', y)
--                |     |
--                |     |
-- (x, y + y') -> +-----+ <- (x + x', y + y')
toPt :: V2 CInt -> Size -> Double -> [Pt]
toPt (V2 x y) (Size (V2 x' y')) rot =
  [ topLeft,
    topRight,
    botLeft,
    botRight
  ]
  where
    topLeft = v2ToTuple $ fmap fromIntegral $ (V2 x y)
    topRight = v2ToTuple $ moveAlongAxis'' (V2 x y) (fromIntegral x' :: Double) ((toRad rot)) :: (Double, Double)
    botLeft = v2ToTuple $ moveAlongAxis'' (V2 x y) (fromIntegral y' :: Double) ((toRad rot) + (toRad 90)) :: (Double, Double)
    botRight = v2ToTuple $ moveAlongAxis'' (fmap floor (tupleToV2 botLeft)) (fromIntegral x') (toRad rot) :: (Double, Double)

collides :: (V2 CInt, Size, Double) -> (V2 CInt, Size, Double) -> Maybe Bool
collides (pos, size, rot) (pos', size', rot') =
  let test = collision 5 a b
   in case test of
        Just True ->
          Tr.trace
            ("Collision 1: " ++ show (toPt pos size rot))
            Tr.trace
            ("Collision 2: " ++ show (toPt pos' size' rot'))
            Tr.trace
            ("Just one last check to collision: " ++ show (collision 5 (toPt pos size rot, polySupport) (toPt pos' size' rot', polySupport)))
            Just
            True
        _ -> test
  where
    a = (toPt pos size rot, polySupport)
    b = (toPt pos' size' rot', polySupport)

-- TODO Make it use a V2 instead
moveAlongAxis :: (Integral a, RealFloat b) => V2 a -> b -> b -> V2 a
moveAlongAxis (V2 x y) dist rot =
  V2 x1 y1
  where
    x1 = x + floor (cos rot * dist)
    y1 = y + floor (sin rot * dist)

-- TODO Make it use a V2 instead
moveAlongAxis' :: (Integral a, RealFloat b) => V2 a -> b -> b -> V2 b
moveAlongAxis' (V2 x y) dist rot =
  V2 x1 y1
  where
    x1 = fromIntegral x + sin rot * dist
    y1 = fromIntegral y + cos rot * dist

moveAlongAxis'' :: (Integral a, RealFloat b) => V2 a -> b -> b -> V2 b
moveAlongAxis'' (V2 x y) dist rot =
  V2 x1 y1
  where
    x1 = fromIntegral $ floor $ fromIntegral x + sin rot * dist
    y1 = fromIntegral $ floor $ fromIntegral y + cos rot * dist

renderToPt sprite todo =
  sequence $
    fmap
      (debugRenderThing sprite (V2 50 50) 0)
      ((fmap . fmap) floor (fmap tupleToV2 (todo :: [(Double, Double)])) :: [V2 CInt])

debugRenderHitbox sprite pos size rot =
  let test = fmap tupleToV2 (toPt pos size rot)
      test2 = fmap (\a -> fmap floor a) test
   in sequence $ fmap (debugRenderThing sprite (V2 20 20) 0) test2

debugRenderThing :: SP.Sprite -> V2 CInt -> CDouble -> V2 CInt -> IO ()
debugRenderThing sprite size rot newPosition =
  SP.renderEx
    sprite
    newPosition
    Nothing
    size
    rot
    (Just (SV.P (V2 0 0)))
    (V2 False False)
