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
                                                , liftM
                                                , forever
                                                )
-- import           Control.Monad.Extra            ( anyM, findM)
import qualified Debug.Trace                   as D

import           Data.Maybe
import           Control.Lens
import           Foreign.C.Types                ( CInt )
import           Control.Concurrent             ( threadDelay )
import           Data.String                    ( fromString )
import           Data.Coerce                    ( coerce )

import qualified Reactive.Banana               as R
import qualified Reactive.Banana.Frameworks    as RF


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

-- *** GameState
data GameState = GameState {_player :: Player}

makeLenses ''GameState

-- * Old basic single key detection
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
  keyboardEventKeyMotion key == Pressed

-- ** Data
data KeyState = KeyState Keycode Bool
  deriving Show

data InputState = InputState {
  _up :: KeyState
  , _down :: KeyState
  , _left :: KeyState
  , _right :: KeyState
            }
  deriving Show

makeLenses ''InputState

myKeys = InputState { _up    = KeyState KeycodeM False
                    , _down  = KeyState KeycodeT False
                    , _left  = KeyState KeycodeS False
                    , _right = KeyState KeycodeN False
                    }

data InputEffect =
  Exit Bool
  | Move (InputState -> GameState -> GameState)

-- ** Input effect
-- *** define input effect
boolToNumber :: Bool -> Maybe CInt
boolToNumber True  = Just 1
boolToNumber False = Nothing

vectorizeMovementGood :: InputState -> GameState -> GameState
vectorizeMovementGood inputState state =
  (player . location)
    `over` ((+) (coerce (vectorizeMovement inputState)))
    $      state

vectorizeMovement :: InputState -> MovementVector
vectorizeMovement (InputState (KeyState keycode1 isPressed1) (KeyState keycode2 isPressed2) (KeyState keycode3 isPressed3) (KeyState keycode4 isPressed4))
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

-- ** Update input state
inputStateUpdate :: [Event] -> InputState -> InputState
inputStateUpdate events state =
  -- D.trace (show (foldr inputStateUpdateSINGLE state events)) $
  foldr inputStateUpdateSINGLE state events

inputStateUpdateSINGLE :: Event -> InputState -> InputState
inputStateUpdateSINGLE event state =
  case (>=>) eventToKeyState (pure . inputStateUpdateTEST state) event of
    Just a  -> a
    Nothing -> state

inputStateUpdateTEST :: InputState -> KeyState -> InputState
inputStateUpdateTEST a@(InputState (KeyState keycode1 isPressed1) (KeyState keycode2 isPressed2) (KeyState keycode3 isPressed3) (KeyState keycode4 isPressed4)) (KeyState newKeyCode newIsPressed)
  = case (newKeyCode == keycode1) of
    True ->
      (InputState (KeyState keycode1 newIsPressed)
                  (KeyState keycode2 isPressed2)
                  (KeyState keycode3 isPressed3)
                  (KeyState keycode4 isPressed4)
      )
    False -> case (newKeyCode == keycode2) of
      True ->
        (InputState (KeyState keycode1 isPressed1)
                    (KeyState keycode2 newIsPressed)
                    (KeyState keycode3 isPressed3)
                    (KeyState keycode4 isPressed4)
        )
      False -> case (newKeyCode == keycode3) of
        True ->
          (InputState (KeyState keycode1 isPressed1)
                      (KeyState keycode2 isPressed2)
                      (KeyState keycode3 newIsPressed)
                      (KeyState keycode4 isPressed4)
          )
        False -> case (newKeyCode == keycode4) of
          True ->
            (InputState (KeyState keycode1 isPressed1)
                        (KeyState keycode2 isPressed2)
                        (KeyState keycode3 isPressed3)
                        (KeyState keycode4 newIsPressed)
            )
          False -> a

-- *** Event -> Keystate
eventToKeyState :: Event -> Maybe KeyState
eventToKeyState = translateEventPayload . eventPayload

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ keysym))
  = Just (KeyState (SDL.keysymKeycode keysym) True)
translateEventPayload (SDL.KeyboardEvent (KeyboardEventData _ Released _ keysym))
  = Just (KeyState (SDL.keysymKeycode keysym) False)
translateEventPayload _ = Nothing

-- * Loop
appLoopNEW :: Renderer -> SDL.Sprite.Sprite -> InputState -> GameState -> IO ()
appLoopNEW renderer sprite inputStateNEWTest state = do
  events <- pollEvents

  -- Rendering
  -- rendererDrawColor renderer $= V4 0 19 48 255
  rendererDrawColor renderer $= V4 0 0 100 255

  let inputStateUpdatedNEW = inputStateUpdate events inputStateNEWTest

  let newState2            = vectorizeMovementGood inputStateUpdatedNEW state

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  clear renderer

  SDL.Sprite.render sprite ((player . location) `view` newState2)

  present renderer

  -- 16 ms frametime = 60 fps
  let ms = 16 :: Float
  threadDelay (round (1000 * ms))

  -- Exit or loop
  unless (isKeyPressed events KeycodeEscape)
         (appLoopNEW renderer sprite inputStateUpdatedNEW newState2)

main :: IO ()
main = do
  initializeAll
  window     <- createWindow (fromString "My SDL Application") defaultWindow

  renderer   <- createRenderer window (-1) defaultRenderer

  spritetest <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)
  appLoopNEW
    renderer
    spritetest
    (InputState (KeyState KeycodeN False)
                (KeyState KeycodeS False)
                (KeyState KeycodeT False)
                (KeyState KeycodeM False)
    )
    (GameState (Player (V2 0 0)))

  -- We might need to destroy more than just the window and renderer in the future
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  -- Crashes GHCI often, but should be used in release
  -- SDL.quit



-- * FRP
-- test2 :: IO ()
test2 :: IO (Maybe [SDL.EventPayload])
test2 = do
  test <- pollEvents
  case fmap eventPayload test of
    [] -> return Nothing
    a  -> return $ Just a


--  Generic Event Source
type EventSource a = (RF.AddHandler a, a -> IO ())

-- test :: RF.MomentIO ()
data SDLEventSource = SDLEventSource { getSDLEvent  :: EventSource EventPayload }

-- test :: RF.MomentIO ()
-- test =
--   do
--     (addHandler, fire) <- RF.newAddHandler
--     RF.register addHandler putStrLn
--     fire "Hello!"

    -- (addHandler, fire) <- RF.newAddHandler
    -- RF.setMouseButtonCallback $ \button _ -> fire button
    -- RF.fromAddHandler addHandler
    -- test <- RF.fromAddHandler test2
    -- undefined

-- registerMouseButton :: IO (eventPayload)
-- registerMouseButton = do
--   (addHandler, fire) <- RF.newAddHandler
--   R.setMouseButtonCallback $ \button _ -> fire button
--   RF.fromAddHandler addHandler
--

echo =
  do
    (keyEventHandler, fire) <- RF.newAddHandler

    -- Network Specification (echo keyboard input)
    -- let networkDescription =
      -- RF.fromAddHandler keyEventHandler >>= -- Create event stream from handler
      -- reactimate . fmap print -- Map print over event stream

    RF.compile networkDescription >>= RF.actuate
      where networkDescription =
        RF.fromAddHandler keyEventHandler >>= -- Create event stream from handler
        reactimate . fmap print -- Map print over event stream


    -- Event Loop
    -- hSetBuffering stdin NoBuffering
    -- undefined >>= fire -- Create keyboad event
    -- forever $ do
    --   ready <- t
    --   if ready
    --     then getChar >>= fire -- Create keyboad event
    --     else return ()
