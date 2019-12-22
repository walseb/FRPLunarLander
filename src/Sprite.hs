{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Sprite where

import Control.Exception
import Control.Monad.IO.Class
import Foreign.C.Types
import qualified SDL.Image
import SDL.Vect
import SDL.Video.Renderer
import Control.Applicative

-- | A loaded sprite.
data Sprite = Sprite
  {
    -- spriteDimensions :: !(V2 CInt)
    -- ^ Render dimensions.
  spriteTexture :: !Texture
    -- ^ The texture to render from.
  , spriteFrame :: !CInt
    -- ^ The current frame index.
  , spriteTextureInfo :: !TextureInfo
    -- ^ Info about the sprite texture.
  , spriteRenderer :: !Renderer
    -- ^ The renderer.
  }

-- | Get the current source rectangle of the sprite's frame.
-- spriteRectangle :: Sprite -> Rectangle CInt
-- spriteRectangle s = Rectangle (P (V2 x 0)) (V2 w h)
--   where (V2 w h) = spriteDimensions s
--         x = spriteFrame s * w

-- | Load a sprite from file.
load :: MonadIO m => Renderer -> FilePath -> V2 CInt -> m Sprite
load ren fp dimensions =
  liftIO
    (do texture <- SDL.Image.loadTexture ren fp
        textInfo <- queryTexture texture
        evaluate
          (Sprite
           { -- spriteDimensions = dimensions
           spriteTexture = texture
           , spriteFrame = 0
           , spriteTextureInfo = textInfo
           , spriteRenderer = ren
           }))

-- | Advance sprite to the next frame.
animate :: Sprite -> CInt -> Sprite
animate s sWidth =
  s
  { spriteFrame =
      if (spriteFrame s + 1) * sWidth < textureWidth (spriteTextureInfo s)
        then spriteFrame s + 1
        else 0
  }
{-# INLINE animate #-}

-- | Render the sprite at the given coordinates.
render :: MonadIO m => Sprite -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> m ()
render s xy sourceRect destSize =
  copy
    (spriteRenderer s)
    (spriteTexture s)
    sourceRect
    (Just (Rectangle (P xy) destSize))
{-# INLINE render #-}

-- | Render the sprite at the given coordinates.
-- Here s is the sprite to be rendered
-- xy is the position of the sprite
-- rot is the rotation of sprite
-- centerRot is the center to rotate around
-- Flip is weather or not to flip the sprite on x and y axis
renderEx :: MonadIO m => Sprite -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx s xy sourceRect destSize rot =
  copyEx
    (spriteRenderer s)
    (spriteTexture s)
    sourceRect
    (Just (Rectangle (P xy) destSize))
    (-rot)
{-# INLINE renderEx #-}

-- Render ex except with distortions based on zoom level and whatever
renderEx' :: MonadIO m => V2 CInt -> V2 CInt -> Sprite -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx' deltaPos zoomLevel s xy sourceRect destSize rot centerRot =
  renderEx
    s
    ((xy + deltaPos) `v2Div` zoomLevel)
    sourceRect
    (destSize `v2Div` zoomLevel)
    rot
    centerRot'
    where
      centerRot' = case centerRot of
                    Just (P a) -> Just $ P $ a `v2Div` zoomLevel
                    Nothing -> Nothing
{-# INLINE renderEx' #-}

v2Div :: (Integral a) => V2 a -> V2 a -> V2 a
v2Div a b = liftA2 div a b
