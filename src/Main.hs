{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

--, threadDelay )

import Collision.GJK
  ( Size (..),
    checkCollisions,
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
import qualified SDL as S
import qualified SDL.Vect as SV
import qualified Sprite as SP
import YampaUtils.Types ()
import Types
import Physics
import Ship

initialGame =
  GameState
    ( Objects
        (Object (V2 0 0) (Size (V2 500 500)) 0 True)
        [(Object (V2 300 800) (Size (V2 500 500)) 0 True)]
    )

applyInputs :: GameState -> Y.SF InputState Objects
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl 0 0 -< (input ^. movement)

  let playerSize = initialGameState ^. (objects . player . size)
  enemyPos <- movingObject (fmap fromIntegral (initialGameState ^. (objects . enemies . to head . pos))) -< (V2 0 (-1))
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
render renderer (Resources sprite) (GameState (Objects player objects), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    case player ^. alive of
      True ->
        SP.renderEx
          sprite
          (player ^. pos)
          Nothing
          (V2 500 500)
          (coerce (player ^. rot))
          (Just (SV.P (V2 0 0)))
          (V2 False False)
      False ->
        pure ()
    SP.render sprite (objects ^. (to head . pos)) Nothing (V2 500 500)
    S.present renderer
    return exit

spritePath :: FilePath
spritePath = "data/testSprite.png"

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") S.defaultWindow
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  spritetest <- SP.load renderer spritePath (V2 500 500)
  let resources = Resources spritetest
  lastInteraction <- newMVar =<< S.time
  let senseInput _canBlock = do
        currentTime <- S.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        events <- Y.Event <$> S.pollEvents
        return (dt, Just events)
  reactimate (return Y.NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
