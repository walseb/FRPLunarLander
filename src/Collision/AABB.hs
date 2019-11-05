module Collision.AABB
  ( Size (..),
    checkCollisions,
  debugRenderHitbox
  )
where

import Foreign.C.Types
import Linear
import Collision.GJK

collisionAABBCheck :: (V2 CInt, Size) -> (V2 CInt, Size) -> Bool
collisionAABBCheck
  -- Box1
  (V2 x0 y0, Size (V2 sizeX0 sizeY0))
  -- Box2
  (V2 x1 y1, Size (V2 sizeX1 sizeY1)) =
    x0 < x1 + sizeX1
      && x0 + sizeX0 > x1
      && y0 < y1 + sizeY1
      && sizeY0 + y0 > y1
