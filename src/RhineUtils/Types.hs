{-# LANGUAGE TypeFamilies #-}

module RhineUtils.Types () where

import FRP.Rhine
import qualified SDL as S

instance (RModule a) => RModule (S.V2 a) where
    type Groundring (S.V2 a) = Groundring a
    zeroVector = S.V2 zeroVector zeroVector
    (S.V2 a b) ^* x = S.V2 (a ^* x) (b ^* x)
    (S.V2 a1 b1) ^+^ (S.V2 a2 b2) = S.V2 (a1 ^+^ a2) (b1 ^+^ b2)

instance (VectorSpace a) => VectorSpace (S.V2 a) where
    (S.V2 a b) ^/ x = S.V2 (a ^/ x) (b ^/ x)

instance (InnerProductSpace a) => InnerProductSpace (S.V2 a) where
    (S.V2 a1 b1) `dot` (S.V2 a2 b2) = (a1 `dot` a2) + (b1 `dot` b2)

instance (NormedSpace a) => NormedSpace (S.V2 a) where
