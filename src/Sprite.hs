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
import Control.Lens
import qualified Debug.Trace as Tr

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
-- spriteRectangle spr = Rectangle (P (V2 x 0)) (V2 w h)
--   where (V2 w h) = spriteDimensions spr
--         x = spriteFrame spr * w

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
animate spr sWidth =
  spr
  { spriteFrame =
      if (spriteFrame spr + 1) * sWidth < textureWidth (spriteTextureInfo spr)
        then spriteFrame spr + 1
        else 0
  }
{-# INLINE animate #-}

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
renderEx :: MonadIO m => Renderer -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx rend spr pos sourceRect destRect theta =
  copyEx
    rend
    spr
    sourceRect
    (Just (Rectangle (P pos) destRect))
    -- This renders the rotation in the wrong way
    theta
{-# INLINE renderEx #-}

-- Render ex except with distortions based on zoom level and whatever
-- deltaPos is the distance between the player and the camera. This means the distance everything needs to be moved by to put the camera at the correct position
renderEx' :: MonadIO m => Renderer -> V2 CInt -> V2 CInt -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx' rend deltaPos zoomLevel spr pos sourceRect destRect theta center =
  -- Tr.trace ("pos is: " ++ show (negateYAxis pos)) $
  renderEx
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
