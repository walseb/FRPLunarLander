module Render.SDL.Render where

import Control.Lens
import FRPEngine.Render.SDL.Primitives
import FRPEngine.Types
import Linear
import qualified SDL as S
import Types

render :: (RealFloat a) => S.Renderer -> Resources -> (GameState a, Bool) -> IO Bool
render renderer res (GameState (CameraState zoomLevel) (PhysicalState (Player _ pCollObj' score' fuel') (Scene terrain' landingSpots')), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer
    renderSpr (pCollObj' ^. obj)
    sequence $ fmap (renderTerr . (^. obj)) terrain'
    sequence $ fmap (renderTerr . (^. lCollObj . obj)) landingSpots'
    renderText' ("score': " ++ show score') (S.Rectangle (S.P (V2 100 100)) (V2 150 100))
    renderText' ("Fuel': " ++ show (floor (if fuel' < 0 then 0 else fuel'))) (S.Rectangle (S.P (V2 100 200)) (V2 150 100))
    S.present renderer
    pure exit
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (pCollObj' ^. (obj . pos)) (flip getSprite res) zoomLevel renderer
    renderSpr = renderObj'
    renderTerr = renderObj'
    renderText' = renderText renderer (res ^. font)
    renderPt pos = renderObj' (Obj pos 0 0 (V2 50 50) SobjectSprite2 True)
