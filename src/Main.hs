{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Control.Lens
import Data.Maybe
import Data.String (fromString)
import FRP.Yampa
import Input.Types as I
import Input.Input
import Linear
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import Types
import Render.SDL.Render
import Collision.Util (ptsApplyObject)
import Actors.Player
import Level

-- accumHoldBy
zoomLevel :: Int -> KeyState -> Int
zoomLevel zoom (ButtonAxisState _ (V2 True _)) = zoom + 1
zoomLevel zoom (ButtonAxisState _ (V2 _ True)) = if zoom - 1 > 0 then zoom -1 else zoom
zoomLevel zoom (ButtonAxisState _ _) = zoom
zoomLevel zoom (ScrollState scrollDist) = if zoom - (fromIntegral scrollDist) > 0 then zoom - (fromIntegral scrollDist) else zoom

applyInputs :: GameState -> SF InputState GameState
applyInputs (GameState (CameraState iZoom) (PhysicalState (MovingState iPlayer iEnemies) scene)) =
  proc input -> do
    (player, enemy) <- (collisionWinSwitch iPlayer iEnemies scene' 0) -< input
    zoomLevel <- accumHoldBy zoomLevel iZoom -< Event (input ^. I.zoom)
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
    applyObjectPosToSceneColl (Scene terr landing) = Scene (fmap ptsApplyObject terr) (fmap (lTerrain `over` ptsApplyObject) landing)
    scene' = applyObjectPosToSceneColl scene

update :: GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( gameState,
      fromJust (newInputState ^. I.quit ^? pressed)
    )

getResources renderer =
  do
    spritetest <- loadTexture renderer spritePath
    spritetest2 <- loadTexture renderer spritePath2
    sceneImg <- loadTexture renderer scenePath
    sceneDangerousImg <- loadTexture renderer sceneDangerousPath
    -- Init fonts
    F.initialize
    font <- F.load fontPath 12
    pure $ Resources font spritetest spritetest2 sceneImg sceneDangerousImg
      where
        loadTexture ren fp = do
          SI.loadTexture ren fp

        spritePath :: FilePath
        spritePath = "data/testSprite.png"

        spritePath2 :: FilePath
        spritePath2 = "data/testSprite2.png"

        scenePath :: FilePath
        scenePath = "data/testTerrain.png"

        sceneDangerousPath :: FilePath
        sceneDangerousPath = "data/testTerrainDangerous.png"

        fontPath :: FilePath
        fontPath = "data/fonts/OpenSans-Regular.ttf"

getSenseInput =
  do
    lastInteraction <- newMVar =<< S.time
    let senseInput _canBlock = do
          currentTime <- S.time
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          events <- Event <$> S.pollEvents
          return (dt, Just events)
    pure senseInput

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer

  resources <- getResources renderer
  senseInput <- getSenseInput

  reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
