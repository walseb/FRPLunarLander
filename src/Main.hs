{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad.Extra            ( anyM )
import qualified Debug.Trace                   as D

import           Data.Maybe
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

  -- We might need to destroy more than just the window and renderer in the future
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  -- Crashes GHCI often, but should be used in release
  -- SDL.quit

-- * Loop
appLoop :: Renderer -> SDL.Sprite.Sprite -> GameState -> IO ()
appLoop renderer sprite state = do
  events <- pollEvents

  -- Rendering
  -- rendererDrawColor renderer $= V4 0 19 48 255
  rendererDrawColor renderer $= V4 0 0 100 255

  let moveDir = vectorizeKeys events
                              (KeycodeM, KeycodeT, KeycodeS, KeycodeN)
                              ((inputState . movement) `view` state)

  let newState1 = (player . location) `over` (+ coerce moveDir) $ state

  let newState2 = (inputState . movement) `set` moveDir $ newState1

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  clear renderer

  SDL.Sprite.render sprite ((player . location) `view` newState2)

  present renderer

  -- 16 ms frametime = 60 fps
  let ms = 16 :: Float
  threadDelay (round (1000 * ms))

  -- Exit
  unless (isKeyPressed events KeycodeEscape) (appLoop renderer sprite newState2)
-- * New way

-- * Old way
-- * Key detection
isKeyPressed :: [Event] -> Keycode -> Bool
isKeyPressed events key =
  any (isJust . fmap isKeyboardEventPressed . eventContainsKey key) events

eventContainsKey :: Keycode -> Event -> Maybe KeyboardEventData
eventContainsKey keycode event = case eventPayload event of
  KeyboardEvent keyboardEvent ->
    case keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode of
      True  -> Just keyboardEvent
      False -> Nothing
  _ -> Nothing

-- Returns "Just True" if key is down and "Just False" if key is up. Otherwise "Nothing"
-- If it's not pressed it has to be released
isKeyboardEventPressed :: KeyboardEventData -> Bool
isKeyboardEventPressed key = -- D.trace (show key)
  (keyboardEventKeyMotion key == Pressed)

-- ** 2 Keys to vector
keyToVector :: [Event] -> Keycode -> Maybe CInt
keyToVector events key =
  case anyM (fmap isKeyboardEventPressed . eventContainsKey key) events of
    Just True  -> D.trace ("press!!!!!!" ++ (show events)) $ Just 1
    Just False -> D.trace ("release!!!!!!" ++ (show events)) $ Just 0
    _          -> Nothing

keysToNumber :: [Event] -> (Keycode, Keycode) -> Maybe CInt
keysToNumber events (key1, key2) = case keyToVector events key1 of
  Just a  -> Just a
  Nothing -> case keyToVector events key2 of
    Just a  -> Just (-a)
    Nothing -> Nothing

-- *** 4 Keys to vector
vectorizeKeys
  :: [Event]
  -> (Keycode, Keycode, Keycode, Keycode)
  -> MovementVector
  -> MovementVector
vectorizeKeys [] _ vect = vect

vectorizeKeys events (vKey1, vKey2, hKey1, hKey2) (MovementVector (V2 a b)) =
    -- Horizontal
  coerce
    (V2 (fromMaybe a (keysToNumber events (hKey2, hKey1)))
    -- Vertical
        (fromMaybe b (keysToNumber events (vKey2, vKey1)))
    )

-- * New
data KeyState = KeyState Keycode Bool

data InputStateNEW = InputStateNEW {
  up :: KeyState
  , down :: KeyState
  , left :: KeyState
  , right :: KeyState
            }

makeLenses ''InputStateNEW

myKeys = InputStateNEW { up    = (KeyState KeycodeM False)
                    , down  = (KeyState KeycodeT False)
                    , left  = (KeyState KeycodeS False)
                    , right = (KeyState KeycodeN False)
                    }

data InputEffect =
  Quit
  | MoveHorizontal CInt
  | MoveVertical CInt

-- ** Event -> InputState -> InputState
inputStateUpdate :: [Event] -> InputStateNEW -> InputStateNEW
inputStateUpdate events state = undefined  -- fmap (\a -> a) $ fmap (inputStateUpdateSingle state) events

inputStateUpdateSingle :: InputStateNEW -> Event -> InputStateNEW
inputStateUpdateSingle event state =
  case (translateEvent event) of
    Just e -> eventDataToKeystate e
    Nothing -> state

-- This will take the input and
eventDataToKeystate :: KeyboardEventData -> Maybe KeyState
eventDataToKeystate (KeyboardEventData _ pressedState _ keysym) = Just (KeyState (SDL.keysymKeycode keysym) (pressedState == SDL.Pressed))
eventDataToKeystate _ = Nothing

extractKeystate :: InputStateNEW -> KeyState -> InputStateNEW
extractKeystate (KeyState key _) inputState =
  getKey (up `view` inputState)

-- ** InputState -> [Effect]

-- ** [Effect] -> World -> World
  -- Use fold here!!!!
-- Endo something..??? Check that out, it's a screenshot on ur desktop
-- applyEffect :: InputEffect -> GameState -> GameState
-- applyEffect effect state = case effect of

-- * Garbage
getKey :: KeyState -> Keycode
getKey (KeyState key _) = key

translateEvent :: Event -> Maybe KeyState
translateEvent = (translateEventPayload . eventPayload)

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (SDL.KeyboardEvent (KeyboardEventData _ isPressed _ keysym)) =
  (KeyState (SDL.keysymKeycode keysym) isPressed)
translateEventPayload _ = Nothing

-- eventToInputEffect :: EventPayload -> Maybe InputEffect
-- eventToInputEffect SDL.QuitEvent         = Just Quit
-- eventToInputEffect (SDL.KeyboardEvent e) = keyToInputEffect e
-- eventToInputEffect _                     = Nothing

-- applyEffect :: InputEffect -> GameState -> GameState
-- applyEffect effect state = case effect of
--   Quit             -> undefined
--   MoveVertical   a -> undefined
--   MoveHorizontal a -> undefined


--applyEffects :: [InputEffect] -> GameState -> GameState
--applyEffects effects state =


-- This is how you get keys from data
-- updateInputState (KeyboardEventData _ SDL.Pressed _ keysym) =
--   case SDL.keysymKeycode keysym of
--     SDL.KeycodeEscape -> Just Quit
--     _                 -> Nothing
-- updateInputState _ = Nothing
