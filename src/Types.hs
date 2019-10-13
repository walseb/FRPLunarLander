{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types () where
import           FRP.Yampa                     as Y

import           SDL

instance (Eq a, Floating a) => Y.VectorSpace (V2 a) a where
    zeroVector = V2 0 0
    a *^ (V2 x y) = V2 (a * x) (a * y)
    (V2 x y) ^/ a = V2 (x / a) (y / a)
    negateVector (V2 x y) = V2 (-x) (-y)
    (V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
    (V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
    (V2 x1 y1) `dot` (V2 x2 y2) = x1 * x2 + y1 * y2
