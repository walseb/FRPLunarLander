{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Collision.GJK
  ( checkCollisions,
  )
import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Control.Lens
import Data.Coerce
import Data.Maybe
import Data.String (fromString)
import Enemy (enemyMovement)
import FRP.Yampa
import Foreign.C.Types
import Input
import Linear
import qualified SDL as S
import qualified SDL.Vect as SV
import Ship (shipControl)
import qualified Sprite as SP
import Types
import YampaUtils.Types ()
import Collision.GJKInternal.Util (toPt)

initialGame =
  GameState
    (CameraState 1)
    ( PhysicalState
        (Living True (Object (V2 1000 0) (V2 500 500) 0))
        [(Living True (Object (V2 0 1200) (V2 500 500) 0))]
    [(Terrain [] (Object (V2 1000 0) (V2 500 500) 0))]
    )

vectorizeMovement :: (RealFloat a) => DirectionalInput -> V2 a
vectorizeMovement
  ( DirectionalInput
      (ButtonState _ a0)
      (ButtonState _ a1)
      (ButtonState _ a2)
      (ButtonState _ a3)
    ) =
    V2
      -- Horizontal
      (boolToInt a3 - boolToInt a2)
      -- Vertical
      (boolToInt a1 - boolToInt a0)
    where
      boolToInt :: (RealFloat a) => Bool -> a
      boolToInt a = if a then 0 else 1
vectorizeMovement _ = error "Trying to vectorize unsupported input"

applyInputs :: GameState -> SF InputState GameState
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl (initialGameState ^. (physicalState . player . lObject . pos)) 0 -< vectorizeMovement (input ^. movement)
  let playerSize = initialGameState ^. (physicalState . player . lObject . size)
  enemyPos <- enemyMovement (initialGameState ^. (physicalState . enemies . to head . lObject . pos)) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (physicalState . enemies . to head . lObject . size)
  isPlayerAlive <- checkCollisions -< ([toPt playerPos playerSize rot], [(toPt enemyPos enemySize 0)])
  returnA -<
     (GameState
          (CameraState (fromMaybe 2 ((input ^. Input.zoom) ^? zoomLevel)))
          (PhysicalState
              -- Player
              (Living isPlayerAlive (Object playerPos playerSize rot))
              -- Enemies
              [(Living True (Object enemyPos enemySize 0))]
              [(Terrain [[(V2 7 7)]] (Object playerPos playerSize rot))]
          )
      )

update :: GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( gameState,
      fromJust (newInputState ^. quit ^? pressed)
    )

-- screenSize = V2 2560 1440
screenSize = V2 1280 720

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite sprite2 scene) (GameState (CameraState zoomLevel) (PhysicalState player objects terrain), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    let screenMiddle = screenSize / 2 * pure zoomLevel
    let dist = screenMiddle - (player ^. (lObject . pos)) - ((player ^. (lObject . size)) / 2)
    let rendSpr = SP.renderEx' (fmap floor dist) (fmap floor (pure zoomLevel))
    case player ^. alive of
      True ->
        rendSpr
          sprite
          (fmap floor (player ^. (lObject . pos)))
          Nothing
          (V2 500 500)
          (coerce (player ^. (lObject . rot)))
          (Just (SV.P (fmap floor ((player ^. (lObject . size)) / 2))))
          (V2 False False)
      False ->
        pure ()
    rendSpr
      sprite
      (floor <$> (objects ^. (to head . lObject . pos)))
      Nothing
      (V2 500 500)
      (coerce (objects ^. (to head . lObject . rot)))
      (Just (SV.P (fmap floor ((objects ^. (to head . lObject . size)) / 2))))
      (V2 False False)
    rendSpr
      scene
      (V2 1000 50)
      Nothing
      ((V2 1920 1080) * 10)
      0
      (Just (SV.P 0))
      (V2 False False)
    S.present renderer
    return exit

spritePath :: FilePath
spritePath = "data/testSprite.png"

spritePath2 :: FilePath
spritePath2 = "data/testSprite2.png"

scenePath :: FilePath
scenePath = "data/testTerrian.png"

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  spritetest <- SP.load renderer spritePath (V2 500 500)
  spritetest2 <- SP.load renderer spritePath2 (V2 500 500)
  sceneImg <- SP.load renderer scenePath (V2 500 500)
  let resources = Resources spritetest spritetest2 sceneImg
  lastInteraction <- newMVar =<< S.time
  let senseInput _canBlock = do
        currentTime <- S.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        events <- Event <$> S.pollEvents
        return (dt, Just events)
  reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
