{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Input where

import Control.Lens
import SDL
import qualified Debug.Trace as Tr

data KeyState
  = ButtonState {_key :: Keycode, _pressed :: Bool}
  | ButtonAxisState {_keyVec :: V2 Keycode, _pressedVec :: V2 Bool}
  | ScrollState {_scrollDist :: Int}
  deriving (Show)

makeLenses ''KeyState
makePrisms ''KeyState

data DirectionalInput
  = DirectionalInput
      { _up :: KeyState,
        _down :: KeyState,
        _left :: KeyState,
        _right :: KeyState
      }
  deriving (Show)

makeLenses ''DirectionalInput

data InputState
  = InputState
      { _zoom :: KeyState,
        _movement :: DirectionalInput,
        _quit :: KeyState
      }
  deriving (Show)

makeLenses ''InputState
makePrisms ''InputState

keybinds = InputState
  { _zoom =
      (ScrollState 0),
      -- (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
    _movement =
      DirectionalInput
        (ButtonState KeycodeM False)
        (ButtonState KeycodeT False)
        (ButtonState KeycodeS False)
        (ButtonState KeycodeN False),
    _quit = ButtonState KeycodeQ False
  }

data CustomEventPayload =
  -- SDL doesn't send a scroll stopped event, therefor we need to create our own
  Ignore |
  ScrollStopped |
  SDLEvent {_payload :: EventPayload}

eventToCustomEventPayload event =
  fmap SDLEvent payload
  -- Add on the scroll stopped event if there aren't any mouse scroll events
  ++ case (or (fmap isMouseWheel payload)) of
      True -> [Ignore]
      False -> [ScrollStopped]
  where
    isMouseWheel (MouseWheelEvent _) = True
    isMouseWheel _ = False
    payload = fmap eventPayload event

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate input events =
  foldr updateKeyInInputState input (eventToCustomEventPayload events)

fromMotion :: InputMotion -> Bool
fromMotion Pressed = True
fromMotion Released = False

replaceKeystate :: CustomEventPayload -> KeyState -> KeyState
-- ButtonState
replaceKeystate (SDLEvent (KeyboardEvent (KeyboardEventData _ pressed _ keysym))) a@(ButtonState key _) = if keysymKeycode keysym == key then ButtonState (keysymKeycode keysym) (fromMotion pressed) else a
-- ButtonAxisState
replaceKeystate
  (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym))) a@(ButtonAxisState key press)
  | keysymKeycode keysym == (key ^. _x) = ButtonAxisState key (V2 (press ^. _x) (fromMotion press0))
  | keysymKeycode keysym == (key ^. _y) = ButtonAxisState key (V2 (fromMotion press0) (press ^. _y))
  | otherwise = a
-- ScrollState
replaceKeystate (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal))) (ScrollState oldScroll) = ScrollState $ oldScroll + fromIntegral (scrollDist ^. _y)
replaceKeystate (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped))) (ScrollState oldScroll) = ScrollState $ oldScroll + (- (fromIntegral (scrollDist ^. _y)))
replaceKeystate (ScrollStopped) (ScrollState _) = ScrollState 0
-- If keys aren't matched or unknown don't do anything to them
replaceKeystate _ a = a

-- We create a new ScrollState every update because there isn't a SDL event for "I have stopped scrolling now"
updateKeyInInputState :: CustomEventPayload -> InputState-> InputState
updateKeyInInputState  event (InputState b1 (DirectionalInput b2 b3 b4 b5) bQuit) =
  InputState
    (replaceKeystate' b1)
    ( DirectionalInput
        (replaceKeystate' b2)
        (replaceKeystate' b3)
        (replaceKeystate' b4)
        (replaceKeystate' b5)
    )
    (replaceKeystate' bQuit)
  where
    replaceKeystate' = replaceKeystate event
