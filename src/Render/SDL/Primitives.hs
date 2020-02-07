{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Render.SDL.Primitives where

import Control.Monad.IO.Class
import Foreign.C.Types
import SDL.Vect
import SDL.Video.Renderer
import Control.Applicative

-- | Render the sprite at the given coordinates.
-- render :: MonadIO m => Sprite -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> m ()
-- render spr pos sourceRect destRect =
--   copy
--     (spriteRenderer spr)
--     (spriteTexture spr)
--     sourceRect
--     (Just (Rectangle (P (-pos)) destRect))
-- {-# INLINE render #-}

-- Render the sprite at the given coordinates.
-- Here spr is the sprite to be rendered
-- pos is the position of the sprite
-- theta is the rotation of sprite
-- center is the center to rotate around
-- Flip is weather or not to flip the sprite on x and y axis
-- sourceRect is the source rectangle to copy, or 'Nothing' for the whole texture
-- destRect is the destination rectangle to copy to, or 'Nothing' for the whole rendering target. The texture will be stretched to fill the given rectangle.
copyEx' :: MonadIO m => Renderer -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
copyEx' rend spr pos sourceRect destRect =
  copyEx
    rend
    spr
    sourceRect
    (Just (Rectangle (P pos) destRect))
{-# INLINE copyEx' #-}

-- Render ex except with distortions based on zoom level and whatever
-- deltaPos is the distance between the player and the camera. This means the distance everything needs to be moved by to put the camera at the correct position
renderEx' :: MonadIO m => Renderer -> V2 CInt -> V2 CInt -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx' rend deltaPos zoomLevel spr pos sourceRect destRect theta center =
  copyEx'
    rend
    spr
    ((pos + deltaPos) `v2Div` zoomLevel)
    sourceRect
    (destRect `v2Div` zoomLevel)
    theta
    center'
    where
      v2Div = liftA2 div
      center' = case center of
                  Just (P c) -> Just $ P $ c `v2Div` zoomLevel
                  Nothing -> Nothing
{-# INLINE renderEx' #-}
