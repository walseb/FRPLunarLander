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
import Enemy (enemyBehavior)
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
import Control.Monad
import qualified Debug.Trace as Tr

initialGame =
  GameState
    (CameraState 1)
    (PhysicalState
        (Living True (Object (V2 1500 (-1000)) (V2 500 500) 0))
        [(Living True (Object (V2 0 1200) (V2 500 500) 0))]
    -- Here the points are relative to the object position. In game loop those gets turned into world position
    (Scene
    ([(Terrain
     [[(V2 0 0.36666666666666664),
       (V2 0.3807291666666667 0.38055555555555554),
       (V2 0.3807291666666667 1),
       (V2 0 1)],

       [(V2 0.3807291666666667 0.7666666666666667),
       (V2 0.7197916666666667 0.7666666666666667),
       (V2 0.7229166666666667 1),
       (V2 0.3807291666666667 1)],

       [(V2 0.7322916666666667 0.3435185185185185),
       (V2 1 0.3675925925925926),
       (V2 1 1),
       (V2 0.7322916666666667 1)]]
      (Object (V2 6000 200) (V2 10 10) 0))


    -- (Terrain
    --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
    --   (Object (V2 0 0) (V2 1 1) 0))
    ])

    ([(Terrain
     [[(V2 0 0.36666666666666664),
       (V2 0.3807291666666667 0.38055555555555554),
       (V2 0.3807291666666667 1),
       (V2 0 1)],

       [(V2 0.3807291666666667 0.7666666666666667),
       (V2 0.7197916666666667 0.7666666666666667),
       (V2 0.7229166666666667 1),
       (V2 0.3807291666666667 1)],

       [(V2 0.7322916666666667 0.3435185185185185),
       (V2 1 0.3675925925925926),
       (V2 1 1),
       (V2 0.7322916666666667 1)]]
      (Object (V2 6000 (-8000)) (V2 5000 5000) 0))

    -- (Terrain
    --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
    --   (Object (V2 0 0) (V2 1 1) 0))
    ]
    )
    )
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
zoomLevel zoom (ScrollState scrollDist) = if zoom - scrollDist > 0 then zoom - scrollDist else zoom

type NonLethalCollision = Bool

-- Bool event refers to if the collision was at lethal velocity or not
livingMovement :: (RealFloat a) => Living -> (V2 a) -> [Living] -> Scene -> SF InputState ((Living, [Living]), Event (NonLethalCollision, Living, [Living]))
livingMovement (Living _ initPlayer) playerVelInit intiEnemies scene = proc input -> do
  (playerObj, playerVel, playerRot) <- shipControl initPlayer (fmap realToFrac playerVelInit) -< vectorizeMovement (input ^. movement)
  enemy <- enemyBehavior (head intiEnemies ^. lObject) -< (V2 0 (-1))
  isPlayerAlive <- checkCollisions -< ([toPt playerObj], [toPt enemy] ++ (join (fmap (^. coll) (scene ^. terrain))))
  isPlayerAlive2 <- checkCollisions -< ([toPt playerObj], join $ (fmap (^. coll) (scene ^. landingSpots)))
  -- returnA -< (Living True playerObj, [(Living True enemy)])
  returnA -<
    ((Living True playerObj, [(Living True enemy)]),
    case isPlayerAlive of
      False ->
        Event (False, (Living False playerObj), [(Living True enemy)])
      True ->
        case isPlayerAlive2 of
          False ->
            case ((sum playerVel) < 1000) && (playerRot < 10) && (playerRot > -10) of
              True -> Event (True, (Living True playerObj), [(Living True enemy)])
              False -> Tr.trace ("rot: " ++ show playerRot) $ Event (False, (Living False playerObj), [(Living True enemy)])
          True -> NoEvent
        )

collisionSwitch :: (RealFloat a ) => Living -> [Living] -> Scene -> V2 a -> SF InputState (Living, [Living])
collisionSwitch player enemies scene playerInitVel =
  switch
    (livingMovement player playerInitVel enemies scene)
    (\d ->
       case d of
         (True, player, enemies') -> collisionSwitch (modifyPlayer player) enemies' scene (modifyVelocity playerInitVel)
         (False, _, _) -> Tr.trace "Dead" $ constant (player, enemies)
       )
  where
    modifyPlayer = (lObject . pos . _y) `over` ((flip (-)) 2)
    modifyVelocity = _y `over` ((flip (-)) 1)

applyInputs :: GameState -> SF InputState GameState
applyInputs initialGameState = proc input -> do
  (playerObj, enemyObjs)
    <- collisionSwitch
         (initialGameState ^. (physicalState . player))
         (initialGameState ^. (physicalState . enemies))
         (Scene (terrains' (getTerrain terrain)) (terrains' (getTerrain landingSpots)))
         0
    -< input

  zoomLevel <- accumHoldBy zoomLevel 3 -< Event (input ^. Input.zoom)
  returnA -<
     (GameState
          (CameraState zoomLevel)
          (PhysicalState
              -- Player
              playerObj
              -- Enemies
              enemyObjs
          (Scene
             (terrains' (getTerrain terrain))
             (terrains' (getTerrain landingSpots)))
          )
      )
  where
   getTerrain getter = initialGameState ^. (physicalState . scene . getter)
   terrainColl' terr' = fmap (\terr -> ptsApplyObject (terr ^. tObject) (terr ^. coll)) terr'
   terrains' terr' = zipWith (\terr coll' ->
                                (Terrain coll' (terr ^. tObject)))
                             terr'
                             (terrainColl' terr')

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
render renderer (Resources sprite sprite2 scene) (game@(GameState (CameraState zoomLevel) (PhysicalState player objects (Scene terrain landingSpots))), exit) =
  do
    -- Tr.traceM ("player pos: " ++ show (player ^. (lObject . pos)))
    -- Tr.traceM ("player rot: " ++ show (player ^. (lObject . rot)))

    S.rendererDrawColor renderer S.$= S.V4 0 0 100 255
    S.clear renderer
    -- Tr.traceM ("Terrain points" ++ (join (join (fmap (\a -> fmap show (a ^. coll)) terrain))))

    let renderDebug = \pos -> renderSpr sprite2 (fmap floor (pos :: V2 Double)) (V2 50 50) 0

    let renderDebug2 = \pos -> renderSpr sprite (fmap floor (pos :: V2 Double)) (V2 50 50) 0
    -- debugHitboxes renderDebug2 (objects ++ [player]) terrain

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

    sequence (fmap
              (\terr ->
                renderScene
                  scene
                  (fmap floor (terr ^. (tObject . pos)))
                  (terr ^. (tObject . size))
                  (coerce (terr ^. (tObject . rot))))
              terrain)
    sequence $ (join . join) $ (fmap . fmap . fmap) renderDebug (fmap (^. coll) terrain)

    sequence (fmap
              (\terr ->
                renderScene
                  scene
                  (fmap floor (terr ^. (tObject . pos)))
                  (terr ^. (tObject . size))
                  (coerce (terr ^. (tObject . rot))))
              landingSpots)
    sequence $ (join . join) $ (fmap . fmap . fmap) renderDebug (fmap (^. coll) landingSpots)

    S.present renderer
    return exit
    where
      screenMiddle = screenSize / 2 * pure (fromIntegral zoomLevel)
      dist = screenMiddle - (negateYAxis (player ^. (lObject . pos))) - (negateYAxis ((player ^. (lObject . size)) / 2))
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


negateYAxis :: (RealFloat a) => V2 a -> V2 a
negateYAxis = _y `over` negate

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
