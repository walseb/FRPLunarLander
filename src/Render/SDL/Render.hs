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

screenSize :: (Integral a) => V2 a
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
render renderer res@(Resources font debugSpr debugSpr2 scene sceneDangerous land1 land2 land3 land4 terr1 terr2 terr3 terr4 terr5) (game@(GameState (CameraState zoomLevel) (PhysicalState (MovingState (Player pObj score fuel) enemies) (Scene terrain landingSpots))), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer
    -- render the ACTUAL position of the pObj
    -- sequence $ fmap renderPt (objToRect (pObj ^. lObj))
    -- sequence $ fmap renderPt (objToRect ((head enemies) ^. lObj))
    case pObj ^. alive of
      True ->
        renderSpr (pObj ^. lObj)
      False ->
        renderText "Game Over" (S.Rectangle (S.P (V2 500 500)) (V2 1000 500))
    sequence $ fmap renderSpr (fmap (^. lObj) enemies)
    sequence $ fmap renderTerr (fmap (^. tObj) terrain)
    sequence $ fmap renderTerr (fmap (^. lTerrain . tObj) landingSpots)
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. coll) terrain)
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. (lTerrain . coll)) landingSpots)

    renderText ("score: " ++ (show score)) (S.Rectangle (S.P (V2 100 100)) (V2 150 100))
    renderText ("Fuel: " ++ (show (floor (if (fuel < 0) then 0 else fuel)))) (S.Rectangle (S.P (V2 100 200)) (V2 150 100))

    S.present renderer
    return exit
  where
    screenMiddle :: (Integral a) => V2 a
    screenMiddle = (liftA2 div screenSize 2) * pure (fromIntegral zoomLevel)
    deltaPos :: V2 Double
    deltaPos = (fmap fromIntegral screenMiddle) - (negateYAxis (pObj ^. (lObj . pos)))
    -- Static stuff center rot at top left
    renderObj' = renderObj deltaPos res (fromIntegral zoomLevel) renderer
    renderSpr = renderObj' True
    renderPt pos = renderObj' True (Object pos (V2 50 50) 0 SobjectSprite2)
    renderTerr = renderObj' False
    renderText text pos = do
      fontSurface <- F.solid font (V4 255 255 255 255) (pack text)
      font <- S.createTextureFromSurface renderer fontSurface
      S.copy renderer font Nothing (Just pos)


negateYAxis :: (Num a) => V2 a -> V2 a
negateYAxis = _y `over` negate
