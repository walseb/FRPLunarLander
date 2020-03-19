module Render.SDL.Render where

import Control.Monad
import Control.Lens
import Linear
import FRPEngine.Render.SDL.Primitives
import qualified SDL as S
import FRPEngine.Types
import Types
import FRPEngine.Collision.Util

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer res (game@(GameState (CameraState zoomLevel) (PhysicalState (MovingState (Player pObj score fuel)) (Scene terrain landingSpots))), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer

    case pObj ^. alive of
      True ->
        renderSpr (pObj ^. lObj)
      False ->
        renderText' "Game Over" (S.Rectangle (S.P (V2 500 500)) (V2 1000 500))

    sequence $ fmap renderTerr (fmap (^. obj) terrain)
    sequence $ fmap renderTerr (fmap (^. lCollObj . obj) landingSpots)

    renderText' ("score: " ++ (show score)) (S.Rectangle (S.P (V2 100 100)) (V2 150 100))
    renderText' ("Fuel: " ++ (show (floor (if (fuel < 0) then 0 else fuel)))) (S.Rectangle (S.P (V2 100 200)) (V2 150 100))

    -- Draw collision nodes
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. coll) terrain)
    -- sequence $ (join . join) $ (fmap . fmap . fmap) renderPt (fmap (^. (lCollObj . coll)) landingSpots)
    -- sequence $ fmap renderPt (objToRect (pObj ^. lObj))--

    S.present renderer
    return exit
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (pObj ^. (lObj . pos)) (flip getSprite res) (fromIntegral zoomLevel) renderer
    renderSpr = renderObj' True
    renderTerr = renderObj' False
    renderText' = renderText renderer (res ^. font)
    renderPt pos = renderObj' True (Object pos (V2 50 50) 0 SobjectSprite2)
