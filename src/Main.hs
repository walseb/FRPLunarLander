{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
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
import           Control.Monad                  ( unless
                                                , (>=>)
                                                )
import           Control.Monad.Extra            ( anyM
                                                , findM
                                                )
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
-- mainOLD :: IO ()
-- mainOLD = do
  -- initializeAll
  -- window     <- createWindow (fromString "My SDL Application") defaultWindow

  -- renderer   <- createRenderer window (-1) defaultRenderer

  -- spritetest <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)
  -- appLoopNEW renderer
  --         spritetest
  --         (InputStateNEW (KeyState KeycodeA True)
  --                          (KeyState KeycodeB False)
  --                          (KeyState KeycodeC False)
  --                          (KeyState KeycodeD False))
  --         (GameState (InputState (MovementVector (V2 0 0))) (Player (V2 0 0)))

  -- -- We might need to destroy more than just the window and renderer in the future
  -- SDL.destroyRenderer renderer
  -- SDL.destroyWindow window
  -- -- Crashes GHCI often, but should be used in release
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
    Just True -> -- D.trace ("press!!!!!!" ++ (show events)) $
      Just 1
    Just False -> -- D.trace ("release!!!!!!" ++ (show events)) $
      Just 0
    _ -> Nothing

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
-- ** Data
data KeyState = KeyState Keycode Bool
  deriving Show

data InputStateNEW = InputStateNEW {
  _up :: KeyState
  , _down :: KeyState
  , _left :: KeyState
  , _right :: KeyState
            }
  deriving Show

makeLenses ''InputStateNEW

myKeys = InputStateNEW { _up    = KeyState KeycodeM False
                       , _down  = KeyState KeycodeT False
                       , _left  = KeyState KeycodeS False
                       , _right = KeyState KeycodeN False
                       }

-- data InputEffect =
--   InputEffect
--   {
--   exit :: Bool
--   , move :: InputStateNEW -> MovementVector -> MovementVector
--   }

data InputEffect =
  Exit Bool
  | Move (InputStateNEW -> GameState -> GameState)

-- ** Input effect
-- *** define input effect
boolToNumber :: Bool -> Maybe CInt
boolToNumber True  = Just 1
boolToNumber False = Nothing

vectorizeMovementGood :: InputStateNEW -> GameState -> GameState
vectorizeMovementGood inputState state = (player . location) `over` ((+) (coerce (vectorizeMovement inputState))) $ state

vectorizeMovement :: InputStateNEW -> MovementVector
vectorizeMovement (InputStateNEW (KeyState keycode1 isPressed1) (KeyState keycode2 isPressed2) (KeyState keycode3 isPressed3) (KeyState keycode4 isPressed4))
  =
    -- Horizontal
    coerce
    (V2
      (case (boolToNumber isPressed1) of
        Just a  -> a
        Nothing -> case (boolToNumber isPressed2) of
          Just b  -> (-b)
          Nothing -> 0
      )
      (case (boolToNumber isPressed3) of
        Just c  -> c
        Nothing -> case (boolToNumber isPressed4) of
          Just d  -> (-d)
          Nothing -> 0
      )
    )

-- *** Use input effect
-- myInputEffect = InputEffect { exit = False, move = vectorizeMovement }

-- ** Apply input effects
-- ** [Event] -> InputStateNEW -> InputStateNEW
test =
  (inputStateUpdateTEST
    (InputStateNEW (KeyState KeycodeA True)
                   (KeyState KeycodeB False)
                   (KeyState KeycodeC False)
                   (KeyState KeycodeD False)
    )
    (KeyState KeycodeD True)
  )

test2 =
  (inputStateUpdateTEST
    (InputStateNEW (KeyState KeycodeA True)
                   (KeyState KeycodeB False)
                   (KeyState KeycodeC False)
                   (KeyState KeycodeD False)
    )
    (KeyState KeycodeS True)
  )


inputStateUpdate :: [Event] -> InputStateNEW -> InputStateNEW
inputStateUpdate events state =
  -- D.trace (show (foldr inputStateUpdateSINGLE state events)) $
  foldr inputStateUpdateSINGLE state events

inputStateUpdateSINGLE :: Event -> InputStateNEW -> InputStateNEW
inputStateUpdateSINGLE event state =
  case (>=>) eventToKeyState (pure . inputStateUpdateTEST state) event of
    Just a  -> a
    Nothing -> state

inputStateUpdateTEST :: InputStateNEW -> KeyState -> InputStateNEW
inputStateUpdateTEST a@(InputStateNEW (KeyState keycode1 isPressed1) (KeyState keycode2 isPressed2) (KeyState keycode3 isPressed3) (KeyState keycode4 isPressed4)) (KeyState newKeyCode newIsPressed)
  = case (newKeyCode == keycode1) of
    True ->
      (InputStateNEW (KeyState keycode1 newIsPressed)
                     (KeyState keycode2 isPressed2)
                     (KeyState keycode3 isPressed3)
                     (KeyState keycode4 isPressed4)
      )
    False -> case (newKeyCode == keycode2) of
      True ->
        (InputStateNEW (KeyState keycode1 isPressed1)
                       (KeyState keycode2 newIsPressed)
                       (KeyState keycode3 isPressed3)
                       (KeyState keycode4 isPressed4)
        )
      False -> case (newKeyCode == keycode3) of
        True ->
          (InputStateNEW (KeyState keycode1 isPressed1)
                         (KeyState keycode2 isPressed2)
                         (KeyState keycode3 newIsPressed)
                         (KeyState keycode4 isPressed4)
          )
        False -> case (newKeyCode == keycode4) of
          True ->
            (InputStateNEW (KeyState keycode1 isPressed1)
                           (KeyState keycode2 isPressed2)
                           (KeyState keycode3 isPressed3)
                           (KeyState keycode4 newIsPressed)
            )
          False ->
            a

inputStateUpdateTEST state _ =
  -- (InputStateNEW (KeyState KeycodeA False)
  --                (KeyState KeycodeA False)
  --                (KeyState KeycodeA False)
  --                (KeyState KeycodeA False)
  -- )
  state


-- *** Event -> Keystate
eventToKeyState :: Event -> Maybe KeyState
eventToKeyState = translateEventPayload . eventPayload

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ keysym))
  = Just (KeyState (SDL.keysymKeycode keysym) True)
translateEventPayload (SDL.KeyboardEvent (KeyboardEventData _ Released _ keysym))
  = Just (KeyState (SDL.keysymKeycode keysym) False)
translateEventPayload _ = Nothing

-- ** GameState -> InputEffect -> GameState
evalEffect :: InputEffect -> GameState -> GameState
evalEffect = undefined

appLoopNEW :: Renderer -> SDL.Sprite.Sprite -> InputStateNEW -> GameState -> IO ()
appLoopNEW renderer sprite inputStateNEWTest state = do
  events <- pollEvents

  -- Rendering
  -- rendererDrawColor renderer $= V4 0 19 48 255
  rendererDrawColor renderer $= V4 0 0 100 255

  let inputStateUpdatedNEW = inputStateUpdate events inputStateNEWTest

  let newState2 = vectorizeMovementGood inputStateUpdatedNEW state

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  clear renderer

  SDL.Sprite.render sprite ((player . location) `view` newState2)

  present renderer

  -- 16 ms frametime = 60 fps
  let ms = 16 :: Float
  threadDelay (round (1000 * ms))

  -- Exit
  unless (isKeyPressed events KeycodeEscape) (appLoopNEW renderer sprite inputStateUpdatedNEW newState2)


main :: IO ()
main = do
  initializeAll
  window     <- createWindow (fromString "My SDL Application") defaultWindow

  renderer   <- createRenderer window (-1) defaultRenderer

  spritetest <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)
  appLoopNEW renderer
          spritetest
        (InputStateNEW (KeyState KeycodeN False)
                           (KeyState KeycodeS False)
                           (KeyState KeycodeT False)
                           (KeyState KeycodeM False))
          (GameState (InputState (MovementVector (V2 0 0))) (Player (V2 0 0)))

  -- We might need to destroy more than just the window and renderer in the future
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  -- Crashes GHCI often, but should be used in release
  -- SDL.quit
