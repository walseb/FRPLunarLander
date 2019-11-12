module Collision.GJKInternal.Support where

import Data.Maybe (fromMaybe)
import Linear (V2(..), dot)
import GJK.Collision
import GJK.Point (Pt)

type Pt' = V2 Double

-- TODO: As you see here I'm redefining this to use V2 but GJK really wants stuff to be in the format (Double, Double) so It's kinda awkward
polySupport' :: [Pt'] -> Pt -> Maybe Pt
polySupport' list (dX, dY) =
    let
        dotList = fmap (dot (V2 dX dY)) list
        decorated = zip dotList list
        maybemax = safeMaximum decorated
    in
        case maybemax of
        Just (_, V2 pX pY) -> Just (pX, pY)
        _  -> Nothing

-- Comes from the GJK package, it doesn't export it so I have to redefine it
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum list = Just $ maximum list

debugIsCollision :: [Pt'] -> [Pt'] -> Bool
debugIsCollision a b =
  fromMaybe False $ collision 1 (a, polySupport') (b, polySupport')
