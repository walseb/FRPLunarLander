{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

import FRP.BearRiver as B
import FRP.Yampa as Y
import Control.Concurrent

import SDL
import qualified SDL.Sprite

import Data.String ( fromString )

import Foreign.C.Types ( CInt )

import Data.Coerce

import qualified Debug.Trace as D

import Control.Lens
 
import           Control.Monad                  ( unless
                                                , (>=>)
                                                ) 

import           Data.Maybe
import           Control.Lens
import           Foreign.C.Types                ( CInt )
import           Control.Concurrent             ( threadDelay )
import           Data.String                    ( fromString )
import           Data.Coerce                    ( coerce )


newtype Player = Player { _location :: V2 CInt }

makeLenses ''Player

-- *** Keys
newtype MovementVector = MovementVector (V2 CInt)

-- *** GameState
data GameState = GameState {_player :: Player}

makeLenses ''GameState

-- * Old basic single key detection
isKeyPressed :: [SDL.Event] -> Keycode -> Bool
isKeyPressed events key =
  any (isJust . fmap isKeyboardEventPressed . eventContainsKey key) events

eventContainsKey :: Keycode -> SDL.Event -> Maybe KeyboardEventData
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
vectorizeMovementGood inputState state = (player . location) `over` ((+) (coerce (vectorizeMovement inputState))) $ state

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
inputStateUpdate :: [SDL.Event] -> InputState -> InputState
inputStateUpdate events state =
  -- D.trace (show (foldr inputStateUpdateSINGLE state events)) $
  foldr inputStateUpdateSINGLE state events

inputStateUpdateSINGLE :: SDL.Event -> InputState -> InputState
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
          False ->
            a

-- *** Event -> Keystate
eventToKeyState :: SDL.Event -> Maybe KeyState
eventToKeyState = translateEventPayload . eventPayload

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (KeyboardEvent (KeyboardEventData _ Pressed _ keysym))
  = Just (KeyState (keysymKeycode keysym) True)
translateEventPayload (KeyboardEvent (KeyboardEventData _ Released _ keysym))
  = Just (KeyState (keysymKeycode keysym) False)
translateEventPayload _ = Nothing

-- * Loop
appLoopNEW :: Renderer -> SDL.Sprite.Sprite -> InputState -> GameState -> IO ()
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

  -- Exit or loop
  unless (isKeyPressed events KeycodeEscape) (appLoopNEW renderer sprite inputStateUpdatedNEW newState2)

main2 :: IO ()
main2 = do
  initializeAll
  window     <- createWindow (fromString "My SDL Application") defaultWindow
  renderer   <- createRenderer window (-1) defaultRenderer

  spritetest <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)
  appLoopNEW renderer
          spritetest
          (InputState (KeyState KeycodeN False)
                             (KeyState KeycodeS False)
                             (KeyState KeycodeT False)
                             (KeyState KeycodeM False))
          (GameState (Player (V2 0 0)))

  -- We might need to destroy more than just the window and renderer in the future
  destroyRenderer renderer
  destroyWindow window
  -- Crashes GHCI often, but should be used in release
  -- quit
  

-- old

type Pos = Double
type Vel = Double

type NewPos = V2 CInt
  

fallingBall :: Pos -> Y.SF () (Pos, Vel)
fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+ y0)) &&& Y.identity)

inputManagement :: InputState -> [SDL.Event] -> InputState
inputManagement iState events = 
  inputStateUpdate events iState 
  -- inputStateUpdate events iState 

todo2 :: IO (Y.Event [SDL.Event])
todo2 = do
  test <- SDL.pollEvents
  return (Y.Event test)

todo :: Y.SF (Y.Event [SDL.Event]) InputState
todo = accumHoldBy inputManagement myKeys

todo4 :: Y.SF (Y.Event [SDL.Event]) (NewPos, Bool)
todo4 = proc events -> do
  test <- todo -< events
  man <- todo5 -< (Y.Event (vectorizeMovement test))
  returnA -< (coerce man, False)

todo5 :: Y.SF (Y.Event MovementVector) NewPos
todo5 = accumHoldBy todo6 (V2 0 0)


todo6 :: NewPos -> MovementVector -> NewPos
todo6 oldPos inputState = 
  oldPos + coerce inputState

spritePath = "data/testSprite.png"

-- -- Exported as abstract type. Fields are accessed with signal functions.
-- data AppInput = AppInput
--     {
--       inpKeyPressed :: Maybe Scancode
--       inpKeyPressed :: Maybe Scancode
--     , inpQuit       :: Bool                    -- ^ SDL's QuitEvent
--     }

-- initAppInput :: AppInput
-- initAppInput = AppInput { inpMousePos   = (0, 0)
--                         , inpMouseLeft  = Nothing
--                         , inpMouseRight = Nothing
--                         , inpKeyPressed = Nothing
--                         , inpQuit       = False
--                         }

main :: IO ()
main = do
  initializeAll

  window     <- createWindow (fromString "My SDL Application") defaultWindow
  renderer   <- createRenderer window (-1) defaultRenderer
  spritetest <- SDL.Sprite.load renderer spritePath (V2 500 500)

  reactimate (return Y.NoEvent)
                  (\ _ -> do
                     -- threadDelay 100000
                     test <- todo2
                     return (0.1, Just test))
                  (\ _ -> render renderer spritetest)
                  todo4
  
  destroyRenderer renderer
  destroyWindow window

render :: SDL.Renderer -> SDL.Sprite.Sprite -> (V2 CInt, Bool) -> IO Bool
render renderer sprite (a, _) =
  do
    D.traceM ("test:" ++ show a)
    rendererDrawColor renderer $= V4 0 0 100 255
    clear renderer 
    SDL.Sprite.render sprite a
    present renderer
    return False
