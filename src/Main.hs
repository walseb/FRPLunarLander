{-# LANGUAGE Arrows #-}

module Main
  ( main,
  )
where

import FRPEngine.Types
import Actors.Player
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import qualified Debug.Trace as Tr
import FRP.Yampa
import FRPEngine.Init
import FRPEngine.Input.Input
import FRPEngine.Input.Types as I
import FRPEngine.Input.Utils
import Level
import Linear
import Render.SDL.Render
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Image as SI
import System.IO.Unsafe
import Types

run :: (RealFloat a) => GameState a -> SF InputState (GameState a, Event (GameState a))
run (GameState (CameraState iZoom) (PhysicalState iPlayer scene)) =
  proc input -> do
    player <- (collisionWinSwitch iPlayer scene (iPlayer ^. (pCollObj . obj . vel))) -< input
    zoomLevel <- accumHoldBy (accumLimit (V2 30 1)) iZoom -< Event (input ^. I.zoom)
    returnA -<
      ( ( GameState
            (CameraState zoomLevel)
            ( PhysicalState
                player
                scene
            )
        ),
        if (player ^. alive) then NoEvent else Event initialGame
      )

type UpdateLoop a = (GameState a -> MVar (GameState a) -> SF (Event [S.Event]) ((GameState a), Bool))

update :: (RealFloat a) => UpdateLoop a
update origGameState mvar = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate defaultKeybinds -< events
  gameState <- runDeathResetSwitch origGameState -< newInputState
  -- This is pretty ugly because if the key type changes, close needs to be changed to pressed for example or else you will get errors. Dependent types probably has a nice solution for this
  let quit = (fromJust (newInputState ^. I.quit ^? close))
      quit' =
        if quit
          then-- UnsafePerformIO has to be used because the default reactimate doesn't allow there to be any self-defined return values on exit
            seq (unsafePerformIO (putMVar mvar gameState)) True
          else False
  returnA -<
    ( gameState,
      quit'
    )
  where
    runDeathResetSwitch :: (RealFloat a) => GameState a -> SF InputState (GameState a)
    runDeathResetSwitch game =
      switch
        (run game)
        (Tr.trace "Dead" $ runDeathResetSwitch)

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

loadOldGameState :: (RealFloat a) => UpdateLoop a -> Maybe (GameState a) -> MVar (GameState a) -> SF (Event [S.Event]) (GameState a, Bool)
loadOldGameState f Nothing =
  f initialGame
loadOldGameState f (Just origGameState) =
  f origGameState

main = do
  myMVar <- newEmptyMVar
  runSDL
    True
    S.Windowed
    "FRP Lunar Lander"
    getResources
    ( \savedGameState renderer senseInput resources -> do
        _ <- reactimate (pure NoEvent) senseInput (\_ -> render renderer resources) (loadOldGameState update (savedGameState :: Maybe (GameState Double)) myMVar)
        mvar <- takeMVar myMVar
        pure mvar
    )
