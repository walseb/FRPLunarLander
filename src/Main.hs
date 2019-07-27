-- * Imports
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified SDL.Sprite
-- Needed for monad loop
-- import           Control.Monad.Loops
import           SDL
import           Linear                         ( V2 )
import           Control.Monad                  ( unless )
import           Control.Lens
import           Foreign.C.Types                ( CInt )
import           Control.Concurrent             ( threadDelay )
import           Data.String                    ( fromString )
import           Data.Coerce                    ( coerce )

-- * Todo
-- Use monad.loops
-- https://stackoverflow.com/questions/19285691/how-do-i-write-a-game-loop-in-haskell
-- http://amixtureofmusings.com/2016/05/19/associated-types-and-haskell/

-- * Types
-- ** State
-- *** Player
newtype Player = Player { _location :: V2 CInt }

makeLenses ''Player

-- *** Keys
newtype MovementVector = MovementVector (V2 CInt)
newtype KeyState = KeyState { _movement :: MovementVector }

makeLenses ''KeyState


-- *** GameState
data GameState = GameState { _keyState :: KeyState,
                             _player :: Player}

makeLenses ''GameState

-- * Commented out stuff
-- -- We have a class interface of player
-- class Player p where
--   playerPosition :: p -> Position
--   playerMoveTo :: Position -> p -> p
--
-- -- We ensure that PlayerData can use the functions defined by player
-- instance Player PlayerData where
--   playerPosition = _pos
--   playerMoveTo pos player = player { _pos = pos }
--
-- -- We have a game state class interface
-- -- class GameState g where
-- --   getPlayer :: Player p => g -> p
-- --   getMonsterPositions :: g -> [Position]
--
-- class Player (PlayerType s) => GameState s where
--   type PlayerType s :: *
--   getPlayer :: s -> PlayerType s
--   getMonsterPositions :: s -> [Position]
--
-- -- The game state data structure consists of the data of a player (notice, not the player class interface, it's not a class mate, it means that that datastructure can use that function interface, so you can't pass it here) and a bunch of positions for enemies
-- data GameStateData = GameStateData PlayerData [Position]
--
-- -- Ensure that GameStateData can use the functions defined in the GameState class
-- -- instance GameState GameStateData where
-- --   getPlayer           (GameStateData p _) = p
-- --   getMonsterPositions (GameStateData _ mPoses) = mPoses
--
-- instance GameState GameStateData where
--   type PlayerType GameStateData = PlayerData
--   getPlayer (GameStateData p _) = p
--   getMonsterPositions (GameStateData _ mPoses) = mPoses
--
-- checkForCollisions :: GameState s => s -> [Position] -> Bool
-- checkForCollisions s ps =
--   let p    = getPlayer s
--       pPos = playerPosition p
--   in  pPos `elem` ps

-- * Main
main :: IO ()
main = do
  initializeAll
  window     <- createWindow (fromString "My SDL Application") defaultWindow

  renderer   <- createRenderer window (-1) defaultRenderer

  spritetest <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)
  appLoop renderer
          spritetest
          (GameState (KeyState (MovementVector (V2 0 0))) (Player (V2 0 0)))

-- * Loop
appLoop :: Renderer -> SDL.Sprite.Sprite -> GameState -> IO ()
appLoop renderer sprite state = do
  events <- pollEvents

  -- Rendering
  rendererDrawColor renderer $= V4 0 0 255 255

  let moveDir = getKeyVector
        events
        (KeycodeUp, KeycodeDown, KeycodeLeft, KeycodeRight)

  let newState = (player . location) `over` (+ coerce moveDir) $ state

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  clear renderer

  SDL.Sprite.render sprite ((player . location) `view` newState)

  present renderer

  -- 16 ms frametime = 60 fps
  let ms = 16 :: Float
  threadDelay (round (1000 * ms))

  -- Exit
  unless (any (isKeyDown KeycodeQ) events) (appLoop renderer sprite newState)


-- * Key detection
isKeyDown :: Keycode -> Event -> Bool
isKeyDown keycode event = case eventPayload event of
    -- If it's a keyboard event, check it further
  KeyboardEvent keyboardEvent ->
    keysymKeycode (keyboardEventKeysym keyboardEvent)
      == keycode
      && keyboardEventKeyMotion keyboardEvent
      == Pressed
  _ -> False

-- ** 2 Keys to vector
vectorizeKeys :: [Event] -> (Keycode, Keycode) -> CInt
vectorizeKeys events (key1, key2) | any (isKeyDown key1) events = 1
                                  | any (isKeyDown key2) events = -1
                                  | otherwise                   = 0

-- *** 4 Keys to vector
getKeyVector
  :: [Event] -> (Keycode, Keycode, Keycode, Keycode) -> MovementVector
getKeyVector events (vKey1, vKey2, hKey1, hKey2) =
    -- Horizontal
                                                   coerce
  (V2 (vectorizeKeys events (hKey2, hKey1))
    -- Vertical
      (vectorizeKeys events (vKey2, vKey1))
  )
