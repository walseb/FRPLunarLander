{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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
import FRP.Rhine
import System.IO.Unsafe

initialGame =
  GameState
    ( Objects
        (Object (V2 800 0) (V2 500 500) 0 True)
        [(Object (V2 800 1200) (V2 500 500) 0 True)]
        -- []
    )

applyInputs :: (Monad m, TimeDomain td, Diff td ~ Double) => GameState -> BehaviourF m td InputState Objects
applyInputs initialGameState = proc input -> do
  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl (initialGameState ^. (objects . player . pos)) 0 -< (input ^. movement)
  let playerSize = initialGameState ^. (objects . player . size)
  enemyPos <- movingObject (initialGameState ^. (objects . enemies . to head . pos)) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (objects . enemies . to head . size)
  isPlayerAlive <- safely checkCollisions -< ((playerPos, playerSize, rot), (enemyPos, enemySize, 0))
  returnA -<
    ( Objects
        -- Player
        (Object playerPos playerSize rot (not isPlayerAlive))
        -- Enemies
        [(Object enemyPos enemySize 0 True)]
    )

update :: (Monad m, TimeDomain td, Diff td ~ Double) => GameState -> BehaviourF m td ([S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumulateWith (flip inputStateUpdate) keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( GameState gameState,
      newInputState ^. (Input.quit . pressed)
    )


-- senseInput myMvar = do
--   currentTime <- S.time
--   dt <- (currentTime -) <$> swapMVar myMvar currentTime
--   events <- S.pollEvents
--   return (dt, Just events)

update' :: (TimeDomain td, Diff td ~ Double) => S.Renderer -> Resources -> a -> GameState -> BehaviourF IO td () ()
update' renderer resources a origGameState = proc events -> do
  -- TODO go to inputStateUpdate and fix it. I reversed argument order
  newInputState <- accumulateWith (flip inputStateUpdate) keybinds -< unsafePerformIO S.pollEvents
  gameState <- applyInputs origGameState -< newInputState
  returnA -< seq (unsafePerformIO (render renderer resources ((GameState gameState), newInputState ^. (Input.quit . pressed)))) ()

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite sprite2) (gameState@(GameState (Objects player objects)), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer

    -- debugHitboxes gameState sprite


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
      False ->
        pure ()

    -- SP.renderEx
    --   sprite
    --   (floor <$> ((player ^. pos) - ((player ^. size) / 2)))
    --   Nothing
    --   (V2 500 500)
    --   (coerce (player ^. rot))
    --   (Just (SV.P (fmap floor ((player ^. size) / 2))))
    --   (V2 False False)

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
  -- reactimate (return Y.NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  -- reactimate senseInput (\_ -> render renderer resources) (update initialGame)
  reactimateCl (waitClock :: Millisecond 10) (update' renderer resources lastInteraction initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
