-- * Imports
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
import           Control.Monad.Extra
import           Data.Maybe
import           Control.Lens
import           Foreign.C.Types                ( CInt )
import           Control.Concurrent             ( threadDelay )
import           Data.String                    ( fromString )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( find )

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
newtype InputState = InputState { _movement :: MovementVector }

makeLenses ''InputState


-- *** GameState
data GameState = GameState { _inputState :: InputState,
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
          (GameState (InputState (MovementVector (V2 0 0))) (Player (V2 0 0)))

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
  unless (any (isEventKeyDown KeycodeQ) events)
         (appLoop renderer sprite newState)


-- * Key detection
-- Really a key can be in 3 states, up down or not pressed this frame
-- If a key is not pressed this frame, return none
-- Else either return True for key is down, or False for key is up
newtype KeyIsDown = KeyIsDown Bool

-- Returns key if key is part of event
eventIsKey :: Keycode -> Event -> Maybe KeyboardEventData
eventIsKey keycode event = case eventPayload event of
  KeyboardEvent keyboardEvent ->
    case keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode of
      True  -> Just keyboardEvent
      False -> Nothing
  _ -> Nothing

-- Returns "Just True" if key is down and "Just False" if key is up. Otherwise "Nothing"
-- If it's not pressed it has to be released
isKeyDown :: KeyboardEventData -> KeyIsDown
isKeyDown key = coerce $ keyboardEventKeyMotion key == Pressed

isKeyPressed :: Keycode -> [Event] -> Maybe KeyIsDown
isKeyPressed key events = findM
  -- If key exists among the events
  (\event -> case (eventIsKey key event) of
    -- Is key that we now know exists
    Just x  -> Just isKeyDown x
    -- Key wasn't pressed
    Nothing -> Nothing
  )
  events

-- ** 2 Keys to vector
-- vectorizeKeys events (key1, key2) | any (isEventKey key1) events = 1
--                                   | any (isEventKey key2) events = -1
--                                   | otherwise                    = 0
keyToVector :: Event -> Keycode -> Maybe CInt
keyToVector event key = case eventIsKey key event of
  Nothing -> Nothing
  Just x ->
    (case coerce isKeyDown x of
      True  -> Just 1
      False -> Just (-1)
    )

-- vectorizeKeys :: [Event] -> (Keycode, Keycode) -> Maybe CInt
-- vectorizeKeys events keys =
--   find (\key -> find (\event -> isJust keyToVector event key) events) keys

vectorizeKeys :: [Event] -> (Keycode, Keycode) -> Maybe CInt
vectorizeKeys events (key1, key2) = find
  (\event -> case keyToVector event key1 of
    Just x  -> x
    Nothing -> case keyToVector event key2 of
      Just x  -> x
      Nothing -> Nothing
  )
  events

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

-- ** Detect key hold
updateKeystate :: [Event] -> InputState -> InputState
updateKeystate = _
