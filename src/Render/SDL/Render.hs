module Render.SDL.Render where

import Collision.Util (objToRect)
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Text (pack)
import Linear
import Render.SDL.Primitives
import qualified SDL as S
import qualified SDL.Font as F
import qualified SDL.Vect as SV
import Types

screenSize :: (RealFloat a) => V2 a
-- screenSize = V2 2560 1440
screenSize = V2 1920 1080
-- screenSize = V2 1280 720

renderObj :: (RealFrac a, Integral b) => V2 a -> Resources -> b -> S.Renderer -> Bool -> Object a -> IO ()
renderObj deltaPos res zoomLevel rend renderFromCenter obj =
  renderSpr
    rend
    (getSprite (obj ^. spr) res)
    (obj ^. pos)
    (obj ^. size)
    (realToFrac (obj ^. rot))
    renderFromCenter
  where
    renderEx' =
      ( \rend rotCenter spr pos size theta ->
          renderEx
            rend
            (fmap floor deltaPos)
            (fmap fromIntegral (pure zoomLevel))
            spr
            (fmap floor (negateYAxis pos))
            Nothing
            (fmap floor size)
            theta
            rotCenter
            (V2 False False)
      )
    renderSpr = \rend spr pos size theta renderFromCenter ->
      renderEx'
        rend
        -- Always rotate around center
        Nothing
        spr
        (if renderFromCenter then (pos - (negateYAxis (size / 2))) else pos)
        size
        theta

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer res@(Resources font debugSpr debugSpr2 scene sceneDangerous land1 land2 land3 land4 terr1 terr2 terr3 terr4 terr5) (game@(GameState (CameraState zoomLevel) (PhysicalState (MovingState (Player player score) enemies) (Scene terrain landingSpots))), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    -- render the ACTUAL position of the player
    -- sequence $ fmap renderPt (objToRect (player ^. lObj))
    -- sequence $ fmap renderPt (objToRect ((head enemies) ^. lObj))
    case player ^. alive of
      True ->
        renderSpr (player ^. lObj)
      False ->
        pure ()
    sequence $ fmap renderSpr (fmap (^. lObj) enemies)
    sequence $ fmap renderTerr (fmap (^. tObj) terrain)
    sequence $ fmap renderTerr (fmap (^. lTerrain . tObj) landingSpots)
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. coll) terrain)
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. (lTerrain . coll)) landingSpots)
    fontSurface <- F.solid font (V4 255 255 255 255) (pack ("score: " ++ (show score)))
    font <- S.createTextureFromSurface renderer fontSurface
    S.copy renderer font Nothing (Just (S.Rectangle (S.P (V2 100 100)) (V2 100 100)))
    S.present renderer
    return exit
  where
    -- screenSize = V2 1280 720
    screenMiddle = (screenSize / 2) * pure (fromIntegral zoomLevel)
    deltaPos :: V2 Double
    deltaPos = screenMiddle - (negateYAxis (player ^. (lObj . pos)))
    -- Static stuff center rot at top left
    renderObj' = renderObj deltaPos res (fromIntegral zoomLevel) renderer
    renderSpr = renderObj' True
    renderPt pos = renderObj' True (Object pos (V2 50 50) 0 SobjectSprite2)
    renderTerr = renderObj' False

negateYAxis :: (Num a) => V2 a -> V2 a
negateYAxis = _y `over` negate
