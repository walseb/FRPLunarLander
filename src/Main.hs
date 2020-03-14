{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Actors.Player
import Collision (ptsApplyObject)
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import FRP.Yampa
import FRPEngine.Input.Interpreter
import FRPEngine.Input.Types as I
import Level
import Linear
import Render.SDL.Render
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import Types
import FRPEngine.Init
import FRPEngine.Input.Input

applyInputs :: GameState -> SF InputState GameState
applyInputs (GameState (CameraState iZoom) (PhysicalState (MovingState iPlayer iEnemies) scene)) =
  proc input -> do
    (player, enemy) <- (collisionWinSwitch iPlayer iEnemies scene' (V2 5000 (-500))) -< input
    zoomLevel <- accumHoldBy (accumLimit (V2 30 1)) iZoom -< Event (input ^. I.zoom)
    returnA -<
      ( GameState
          (CameraState zoomLevel)
          ( PhysicalState
              ( MovingState
                  -- Player
                  player
                  -- Enemies
                  enemy
              )
              scene'
          )
      )
  where
    applyObjectPosToSceneColl (Scene terr landing) = Scene (fmap ptsApplyObject terr) (fmap (lCollObj `over` ptsApplyObject) landing)
    scene' = applyObjectPosToSceneColl scene

update :: GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate defaultKeybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( gameState,
      fromJust (newInputState ^. I.quit ^? pressed)
    )

getResources :: (MonadIO m) => S.Renderer -> m Resources
getResources renderer =
  -- Init fonts
  Resources
    <$> (F.initialize >> F.load fontPath 12)
    <*> load hiddenPath renderer
    <*> load spritePath renderer
    <*> load spritePath2 renderer
    <*> load scenePath renderer
    <*> load sceneDangerousPath renderer
    <*> load land1 renderer
    <*> load land2 renderer
    <*> (load land3 renderer)
    <*> (load land4 renderer)
    <*> (load terr1 renderer)
    <*> (load terr2 renderer)
    <*> (load terr3 renderer)
    <*> (load terr4 renderer)
    <*> (load terr5 renderer)
  where
    load :: (MonadIO m) => FilePath -> S.Renderer -> m S.Texture
    load path rend = SI.loadTexture rend path
    hiddenPath = "data/hidden.png"
    spritePath = "data/player.png"
    spritePath2 = "data/testSprite2.png"
    scenePath = "data/testTerrain.png"
    sceneDangerousPath = "data/testTerrainDangerous.png"
    fontPath = "data/fonts/OpenSans-Regular.ttf"
    land1 = "data/maps/land1.png"
    land2 = "data/maps/land2.png"
    land3 = "data/maps/land3.png"
    land4 = "data/maps/land4.png"
    terr1 = "data/maps/terr1.png"
    terr2 = "data/maps/terr2.png"
    terr3 = "data/maps/terr3.png"
    terr4 = "data/maps/terr4.png"
    terr5 = "data/maps/terr5.png"

main =
  runSDL
    getResources
    (\renderer senseInput resources -> reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame))
