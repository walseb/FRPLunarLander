module Render.Render where

import Collision.GJKInternal.Util (toPt)
import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Text (pack)
import Foreign.C.Types
import Linear
import qualified SDL as S
import qualified SDL.Font as F
import qualified SDL.Vect as SV
import Render.Primitives (renderEx')
import Types

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources font sprite sprite2 scene sceneDangerous) (game@(GameState (CameraState zoomLevel) (PhysicalState (MovingState (Player player score) objects) (Scene terrain landingSpots))), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    let renderDebug = \pos -> renderSpr renderer sprite2 pos (V2 50 50) 0
    let renderDebug2 = \pos -> renderSpr renderer sprite pos (V2 50 50) 0
    -- render the ACTUAL position of the player
    sequence $ fmap renderDebug (toPt (player ^. lObject))
    sequence $ fmap renderDebug (toPt ((head objects) ^. lObject))
    case player ^. alive of
      True ->
        renderSpr
          renderer
          sprite
          (player ^. (lObject . pos))
          -- (V2 500 500)
          (player ^. (lObject . size))
          (coerce (player ^. (lObject . rot)))
      False ->
        pure ()
    renderSpr
      renderer
      sprite
      (objects ^. (to head . lObject . pos))
      -- (V2 500 500)
      (fmap coerce (objects ^. (to head . lObject . size)))
      (coerce (objects ^. (to head . lObject . rot)))
    sequence
      ( fmap
          ( \terr ->
              renderScene
                renderer
                sceneDangerous
                (terr ^. (tObject . pos))
                (terr ^. (tObject . size))
                (coerce (terr ^. (tObject . rot)))
          )
          terrain
      )
    sequence $ (join . join) $ (fmap . fmap . fmap) renderDebug (fmap (^. coll) terrain)
    sequence
      ( fmap
          ( \terr ->
              renderScene
                renderer
                scene
                (terr ^. (tObject . pos))
                (terr ^. (tObject . size))
                (coerce (terr ^. (tObject . rot)))
          )
          (fmap (^. lTerrain) landingSpots)
      )
    sequence $ (join . join) $ (fmap . fmap . fmap) renderDebug (fmap (^. (lTerrain . coll)) landingSpots)
    fontSurface <- F.solid font (V4 255 255 255 255) (pack ("score: " ++ (show score)))
    font <- S.createTextureFromSurface renderer fontSurface
    S.copy renderer font Nothing (Just (S.Rectangle (S.P (V2 100 100)) (V2 100 100)))
    S.present renderer
    return exit
  where
    screenSize :: (RealFloat a) => V2 a
    screenSize = V2 2560 1440
    -- screenSize = V2 1280 720
    screenMiddle = (screenSize / 2) * pure (fromIntegral zoomLevel)
    deltaPos = screenMiddle - (negateYAxis (player ^. (lObject . pos)))
    copy =
      ( \rend center spr pos size theta ->
          renderEx'
            rend
            (fmap floor deltaPos)
            (fmap fromIntegral (pure zoomLevel))
            spr
            (fmap floor (negateYAxis pos))
            Nothing
            (fmap floor size)
            theta
            center
            (V2 False False)
      )
    renderSpr = \rend spr pos size rot -> copy rend (Just (SV.P (fmap floor (size / 2)))) spr (pos - (negateYAxis (size / 2))) size rot
    -- Static stuff center rot at top left
    renderScene rend spr pos size = copy rend Nothing spr pos size

negateYAxis :: (Num a) => V2 a -> V2 a
negateYAxis = _y `over` negate
