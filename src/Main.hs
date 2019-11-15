{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

--, threadDelay )

import Collision.GJK
  ( checkCollisions,
  )
import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Control.Lens
import Data.Coerce
import Data.String (fromString)
import FRP.BearRiver as B
import FRP.Yampa as Y
import Foreign.C.Types
import Input
import Linear
import Physics (movingObject)
import qualified SDL as S
import qualified SDL.Vect as SV
import Ship (shipControl)
import qualified Sprite as SP
import Types
import Collision.GJKInternal.Debug
import YampaUtils.Types ()

initialGame =
  GameState
    ( Objects
        (Object (V2 1000 500) (V2 500 500) 0 True)
        [(Object (V2 999999 999999) (V2 500 500) 0 True)]
    )

applyInputs :: GameState -> Y.SF InputState Objects
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl (initialGameState ^. (objects . player . pos)) 0 -< (input ^. movement)
  let playerSize = initialGameState ^. (objects . player . size)
  enemyPos <- movingObject (initialGameState ^. (objects . enemies . to head . pos)) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (objects . enemies . to head . size)
  isPlayerAlive <- checkCollisions -< ((playerPos, playerSize, rot), (enemyPos, enemySize, 0))
  returnA -<
    ( Objects
        -- Player
        (Object playerPos playerSize rot isPlayerAlive)
        -- Enemies
        [(Object enemyPos enemySize 0 True)]
    )

update :: GameState -> Y.SF (Y.Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( GameState gameState,
      newInputState ^. (Input.quit . pressed)
    )

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite sprite2) (GameState (Objects player objects), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    case player ^. alive of
      True ->
        SP.renderEx
          sprite
          (floor <$> ((player ^. pos) - ((player ^. size) / 2)))
          Nothing
          (V2 500 500)
          (coerce (player ^. rot))
          (Just (SV.P (fmap floor ((player ^. size) / 2))))
          (V2 False False)
        -- pure ()
      False ->
        pure ()

    SP.renderEx
      sprite
      (floor <$> ((objects ^. (to head . pos)) - ((objects ^. (to head . size)) / 2)))
      Nothing
      (V2 500 500)
      (coerce (objects ^. (to head . rot)))
      (Just (SV.P (fmap floor ((objects ^. (to head . size)) / 2))))
      (V2 False False)

    S.present renderer
    return exit

spritePath :: FilePath
spritePath = "data/testSprite.png"

spritePath2 :: FilePath
spritePath2 = "data/testSprite2.png"

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  spritetest <- SP.load renderer spritePath (V2 500 500)
  spritetest2 <- SP.load renderer spritePath2 (V2 500 500)
  let resources = Resources spritetest spritetest2
  lastInteraction <- newMVar =<< S.time
  let senseInput _canBlock = do
        currentTime <- S.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        events <- Y.Event <$> S.pollEvents
        return (dt, Just events)
  reactimate (return Y.NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
