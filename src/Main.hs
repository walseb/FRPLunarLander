{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import Collision.GJK
import Collision.GJKInternal.Util (ptsApplyObject, toPt)
import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Maybe
import Data.String (fromString)
import qualified Debug.Trace as Tr
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
import qualified SDL.Font as F
import Data.Text (pack)
import SDL.Image as SI

type Score = Int

initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( MovingState
            (Player (Living True (Object (V2 0 0) (V2 500 500) 0)) 0)
            [(Living True (Object (V2 0 1200) (V2 500 500) 0))]
        )
        -- Here the points are relative to the object position. In game loop those gets turned into world position
        ( Scene
            ( [ ( Terrain
                    ( (fmap . fmap)
                        negateYAxis
                        [ [ (V2 0 0.36666666666666664),
                            (V2 0.3807291666666667 0.38055555555555554),
                            (V2 0.3807291666666667 1),
                            (V2 0 1)
                          ],
                          [ (V2 0.3807291666666667 0.7666666666666667),
                            (V2 0.7197916666666667 0.7666666666666667),
                            (V2 0.7229166666666667 1),
                            (V2 0.3807291666666667 1)
                          ],
                          [ (V2 0.7322916666666667 0.3435185185185185),
                            (V2 1 0.3675925925925926),
                            (V2 1 1),
                            (V2 0.7322916666666667 1)
                          ]
                        ]
                    )
                    (Object (V2 3000 0) (V2 5000 5000) 0)
                )
                -- (Terrain
                --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
                --   (Object (V2 0 0) (V2 1 1) 0))
              ]
            )
            ( [ ( LandingSpot
                    3
                    ( Terrain
                        ( (fmap . fmap)
                            negateYAxis
                            [ [ (V2 0 0.36666666666666664),
                                (V2 0.3807291666666667 0.38055555555555554),
                                (V2 0.3807291666666667 1),
                                (V2 0 1)
                              ],
                              [ (V2 0.3807291666666667 0.7666666666666667),
                                (V2 0.7197916666666667 0.7666666666666667),
                                (V2 0.7229166666666667 1),
                                (V2 0.3807291666666667 1)
                              ],
                              [ (V2 0.7322916666666667 0.3435185185185185),
                                (V2 1 0.3675925925925926),
                                (V2 1 1),
                                (V2 0.7322916666666667 1)
                              ]
                            ]
                        )
                        (Object (V2 (-2000) 0) (V2 5000 5000) 0)
                    )
                )
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
      (boolToInt a2 - boolToInt a3)
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
zoomLevel zoom (ScrollState scrollDist) = if zoom - (fromIntegral scrollDist) > 0 then zoom - (fromIntegral scrollDist) else zoom

type NonLethalCollision = Bool

-- Bool event refers to if the collision was at lethal velocity or not
livingMovement :: (RealFloat a) => Living -> (V2 a) -> [Living] -> Scene -> SF InputState ((Living, [Living]), Event (Maybe (NonLethalCollision, Living, [Living], V2 a)))
livingMovement (Living _ initPlayer) playerVelInit intiEnemies scene = proc input -> do
  (playerObj, playerVel, playerRot) <- shipControl initPlayer (fmap realToFrac playerVelInit) -< vectorizeMovement (input ^. movement)
  enemy <- enemyBehavior (head intiEnemies ^. lObject) -< (V2 0 (-1))
  let playerCollision = collidesWrap scene (MovingState (Player (Living True playerObj) 0) [(Living True enemy)])
  -- returnA -< (Living True playerObj, [(Living True enemy)])
  returnA -<
    ( (Living True playerObj, [(Living True enemy)]),
      case playerCollision of
        Nothing -> NoEvent
        Just False -> Event Nothing
        Just True ->
          case (sum (abs playerVel) < 1000) && (playerRot < 10) && (playerRot > -10) of
            True -> Event $ Just (True, (Living True playerObj), [(Living True enemy)], fmap realToFrac playerVel)
            False -> Event Nothing
    )

livingMovementScore :: (RealFloat a) => Player -> (V2 a) -> [Living] -> Scene -> SF InputState ((Player, [Living]), Event (Maybe (NonLethalCollision, Player, [Living], V2 a)))
livingMovementScore p@(Player (Living _ iPlayerObj) _) playerVelInit intiEnemies scene = proc input -> do
  (playerObj, playerVel, playerRot) <- shipControl iPlayerObj (fmap realToFrac playerVelInit) -< vectorizeMovement (input ^. movement)
  enemy <- enemyBehavior (head intiEnemies ^. lObject) -< (V2 0 (-1))
  let player' = (((pLiving . lObject) .~ playerObj) p)
  let playerCollision = collidesWrap scene (MovingState player' [(Living True enemy)])
  -- returnA -< (Living True playerObj, [(Living True enemy)])
  returnA -<
    ( (player', [(Living True enemy)]),
      case playerCollision of
        Nothing -> NoEvent
        Just False -> Event Nothing
        Just True ->
          case (sum (abs playerVel) < 1000) && (playerRot < 10) && (playerRot > -10) of
            True -> Event $ Just (True, player', [(Living True enemy)], fmap realToFrac playerVel)
            False -> Event Nothing
    )

-- Completely solves collision by moving the player up and re-trying collision test
solveCollision :: Scene -> MovingState -> Player
solveCollision scene (MovingState player enemies) =
  case collidesWrap scene (MovingState player enemies) of
    Just True -> solveCollision scene (MovingState (((pLiving . lObject . pos . _y) `over` (+ 8)) player) enemies)
    Just False -> (Player (Living False (player ^. pLiving . lObject)) (player ^. score))
    Nothing -> player

-- Return the score value of the object hit
solveCollisionScore :: Scene -> MovingState -> Player
solveCollisionScore scene (MovingState player' enemies) =
  case collidesWrapScore scene (MovingState player' enemies) of
    Just (True, score') ->
      let score'' = player' ^. score in
        (Player (initialGame ^. (physicalState . movingState . player . pLiving)) (score' + score''))
    Just (False, _) -> ((pLiving . alive) .~ False) player'
    Nothing -> player'

collisionSwitch :: (RealFloat a) => Player -> [Living] -> Scene -> V2 a -> SF InputState (Living, [Living])
collisionSwitch (Player player score) enemies scene playerInitVel =
  switch
    (livingMovement player playerInitVel enemies scene)
    ( \d ->
        case d of
          (Just (True, player', enemies', playerVel)) -> collisionSwitch (solveCollision scene (MovingState (Player player' 0) enemies')) enemies' scene (playerVel / 2)
          Nothing -> constant (player, enemies)
    )

-- Same as collisionSwitch except instead of keeping the player from falling through object on proper non-lethal collision with landing spot it awards the player with points
collisionWinSwitch :: (RealFloat a) => Player -> [Living] -> Scene -> V2 a -> SF InputState (Player, [Living])
collisionWinSwitch player enemies scene playerInitVel =
  switch
    (livingMovementScore player playerInitVel enemies scene)
    ( \d ->
        case d of
          (Just (True, player', enemies', playerVel)) -> collisionWinSwitch (solveCollisionScore scene (MovingState player' enemies')) enemies' scene (playerVel / 2)
          Nothing -> constant (player, enemies)
    )

applyInputs :: GameState -> SF InputState GameState
applyInputs (GameState (CameraState iZoom) (PhysicalState (MovingState iPlayer iEnemies) scene)) =
  proc input -> do
    (player, enemy) <- (collisionWinSwitch iPlayer iEnemies scene' 0) -< input
    zoomLevel <- accumHoldBy zoomLevel iZoom -< Event (input ^. Input.zoom)
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
    applyObjectPosToSceneColl (Scene terr landing) = (Scene (fmap ptsApplyObject terr) (fmap (lTerrain `over` ptsApplyObject) landing))
    scene' = (applyObjectPosToSceneColl scene)

update :: GameState -> SF (Event [S.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events
  gameState <- applyInputs origGameState -< newInputState
  returnA -<
    ( gameState,
      fromJust (newInputState ^. Input.quit ^? pressed)
    )

screenSize :: (RealFloat a) => V2 a
screenSize = V2 2560 1440
-- screenSize = V2 1280 720

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
    screenMiddle = (screenSize / 2) * pure (fromIntegral zoomLevel)
    deltaPos = screenMiddle - (negateYAxis (player ^. (lObject . pos)))
    copy =
      ( \rend center spr pos size theta ->
          SP.renderEx'
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

-- negateYAxis = id

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

myLoad :: S.Renderer -> FilePath -> IO S.Texture
myLoad ren fp = do
  texture <- SI.loadTexture ren fp
  return texture

main :: IO ()
main = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  spritetest <- myLoad renderer spritePath
  spritetest2 <- myLoad renderer spritePath2
  sceneImg <- myLoad renderer scenePath
  sceneDangerousImg <- myLoad renderer sceneDangerousPath


  -- Fonts
  F.initialize
  font <- F.load fontPath 12

  let resources = Resources font spritetest spritetest2 sceneImg sceneDangerousImg
  lastInteraction <- newMVar =<< S.time
  let senseInput _canBlock = do
        currentTime <- S.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        events <- Event <$> S.pollEvents
        return (dt, Just events)
  reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame)
  S.destroyRenderer renderer
  S.destroyWindow window
