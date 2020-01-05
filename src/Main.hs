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
import Collision.GJKInternal.Util (toPt, ptsApplyObject)
import Collision.GJKInternal.Debug
import Control.Monad

initialGame =
  GameState
    (CameraState 1)
    (PhysicalState
        (Living True (Object (V2 1500 (-1000)) (V2 500 500) 90))
        [(Living True (Object (V2 0 1200) (V2 500 500) 0))]
    -- Here the points are relative to the object position. In game loop those gets turned into world position
    [(Terrain
      [[(V2 0 0), (V2 500 0), (V2 500 500), (V2 0 500)]]
      (Object (V2 200 200) (V2 1 1) 0))
    -- (Terrain
    --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
    --   (Object (V2 0 0) (V2 1 1) 0))
    ]
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

-- accumHoldBy
zoomLevel :: Int -> KeyState -> Int
zoomLevel zoom (ButtonAxisState _ (V2 True _)) = zoom + 1
zoomLevel zoom (ButtonAxisState _ (V2 _ True)) = if zoom - 1 > 0 then zoom -1 else zoom
zoomLevel zoom (ButtonAxisState _ _) = zoom
zoomLevel zoom (ScrollState scrollDist) = if zoom + scrollDist > 0 then zoom + scrollDist else zoom

--   (3 +
--    3)
-- test = (fmap (+1)[3,2,1])
--   test
-- test = (fmap
--         ([[(V2 8 8), (V2 8 8)], [(V2 8 8), (V2 8 8)]] ++)
--         [[[(V2 8 8), (V2 8 8)], [(V2 8 8), (V2 8 8)]],
--          [[(V2 8 8), (V2 8 8)], [(V2 8 8), (V2 8 8)]]])

applyInputs :: GameState -> SF InputState GameState
applyInputs initialGameState = proc input -> do
  -- TODO: Problem here is that it only checks for 1 terrain object
  -- let terrainColl = initialGameState ^. (physicalState . terrain . to head . coll)

  -- Problem here: It doesn't apply size
  -- let terrainColl' = fmap
  --                      ptsApplyObject
  --                      (fmap (^. tObject) (initialGameState ^. (physicalState . terrain)))
  --                    <*>
  --                      fmap (^. coll)
  --                      (initialGameState ^. (physicalState . terrain))

  let terrains = (initialGameState ^. physicalState . terrain)
  let terrainColl' = fmap (\terr -> ptsApplyObject (terr ^. tObject) (terr ^. coll)) terrains

  let terrains' = (zipWith (\terr coll' -> (Terrain coll'
                                            -- Ok so here we create a new empty object BECAUSE the rot,pos,etc etc has already been applied to the collision vertices above! It's static
                                            (Object 0 1 0)
                                           )) terrains terrainColl')
  -- let terrains' = (zipWith (\terr coll' -> (Terrain coll' (terr ^. tObject))) terrains terrainColl')

  -- Calculate new pos without modifying state
  (rot, playerPos) <- shipControl (initialGameState ^. (physicalState . player . lObject . pos)) (initialGameState ^. (physicalState . player . lObject . rot)) 0 -< vectorizeMovement (input ^. movement)
  let playerSize = initialGameState ^. (physicalState . player . lObject . size)
  enemyPos <- enemyMovement (initialGameState ^. (physicalState . enemies . to head . lObject . pos)) -< (V2 0 (-1))
  let enemySize = initialGameState ^. (physicalState . enemies . to head . lObject . size)
  isPlayerAlive <- checkCollisions -< ([toPt playerPos playerSize rot], ([toPt enemyPos enemySize 0] ++) (join terrainColl'))
  zoomLevel <- accumHoldBy zoomLevel 3 -< Event (input ^. Input.zoom)
  returnA -<
     (GameState
          (CameraState zoomLevel)
          (PhysicalState
              -- Player
              (Living isPlayerAlive (Object playerPos playerSize rot))
              -- Enemies
              [(Living True (Object enemyPos enemySize 0))]
             terrains'
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

screenSize = V2 2560 1440
-- screenSize = V2 1280 720

-- debugRender game sprite2 renderSpr pos = debugHitboxes (renderSpr sprite2 ((fmap floor (pos :: V2 Double)) :: V2 CInt) (V2 100 100)) game

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite sprite2 scene) (game@(GameState (CameraState zoomLevel) (PhysicalState player objects terrain)), exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    -- Tr.traceM ("Terrain points" ++ (join (join (fmap (\a -> fmap show (a ^. coll)) terrain))))

    let renderDebug = \pos -> renderSpr sprite2 (fmap floor (pos :: V2 Double)) (V2 50 50) 0
    let renderDebug2 = \pos -> renderSpr sprite (fmap floor (pos :: V2 Double)) (V2 50 50) 0
    -- debugHitboxes renderDebug2 (objects ++ [player]) terrain

    sequence $ (join . join) $ (fmap . fmap . fmap) renderDebug (fmap (^. coll) terrain)

    -- render the ACTUAL position of the player
    -- sequence $ fmap renderDebug (toPt (player ^. (lObject . pos)) (player ^. (lObject . size)) (player ^. (lObject . rot)))

    case player ^. alive of
      True ->
        renderSpr
          sprite
          (fmap floor (player ^. (lObject . pos) - (player ^. (lObject . size) / 2)))
          -- (V2 500 500)
          (player ^. (lObject . size))
          (coerce (player ^. (lObject . rot)))
      False ->
        pure ()
    renderSpr
      sprite
      (floor <$> (objects ^. (to head . lObject . pos)) - ((objects ^. (to head . lObject . size)) / 2))
      -- (V2 500 500)
      (fmap coerce (objects ^. (to head . lObject . size)))
      (coerce (objects ^. (to head . lObject . rot)))
    renderScene
      scene
      (V2 1000 50)
      ((V2 1920 1080) * 10)
      0
    S.present renderer
    return exit
    where
      screenMiddle = screenSize / 2 * pure (fromIntegral zoomLevel)
      dist = screenMiddle - (player ^. (lObject . pos)) - ((player ^. (lObject . size)) / 2)
      render = (\center spr pos size theta ->
                    SP.renderEx'
                      (fmap floor dist)
                      (fmap fromIntegral (pure zoomLevel))
                      spr
                      pos
                      Nothing
                      (fmap floor size)
                      theta
                      center
                      (V2 False False))
      renderSpr = \spr pos size rot -> render (Just (SV.P (fmap floor (size / 2)))) spr pos size rot
      -- Static stuff center rot at top left
      renderScene = render Nothing

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
