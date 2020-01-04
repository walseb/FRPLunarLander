{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Input where

import Control.Lens
import SDL

data KeyState
  = ButtonState {_key :: Keycode, _pressed :: Bool}
  | ButtonAxisState {_keyVec :: V2 Keycode, _pressedVec :: V2 Bool}
  | ScrollState {_scrollDist :: Int}
  deriving (Show)

makePrisms ''KeyState

makeLenses ''KeyState

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

keybinds = InputState
  { _zoom =
      -- (ScrollState 0),
      (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
    _movement =
      DirectionalInput
        (ButtonState KeycodeM False)
        (ButtonState KeycodeT False)
        (ButtonState KeycodeS False)
        (ButtonState KeycodeN False),
    _quit = ButtonState KeycodeQ False
  }

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate = foldr (updateKeyInInputState . eventPayload)

fromMotion :: InputMotion -> Bool
fromMotion Pressed = True
fromMotion Released = False

replaceKeystate :: EventPayload -> KeyState -> KeyState
-- ButtonState
replaceKeystate (KeyboardEvent (KeyboardEventData _ pressed _ keysym)) a@(ButtonState key _) = if keysymKeycode keysym == key then ButtonState (keysymKeycode keysym) (fromMotion pressed) else a
-- ButtonAxisState
replaceKeystate
  (KeyboardEvent (KeyboardEventData _ press0 _ keysym)) a@(ButtonAxisState key press)
  | keysymKeycode keysym == (key ^. _x) = ButtonAxisState key (V2 (press ^. _x) (fromMotion press0))
  | keysymKeycode keysym == (key ^. _y) = ButtonAxisState key (V2 (fromMotion press0) (press ^. _y))
  | otherwise = a
-- ScrollState
replaceKeystate (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal)) (ScrollState oldScrollDist) =
  ScrollState (fromIntegral (scrollDist ^. _y))
replaceKeystate (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped)) (ScrollState oldScrollDist) =
  ScrollState (- (fromIntegral (scrollDist ^. _y)))
-- If keys aren't matched or unknown don't do anything
replaceKeystate _ a = a

updateKeyInInputState ::   EventPayload -> InputState-> InputState
updateKeyInInputState  event (InputState zoomLevelDelta (DirectionalInput b2 b3 b4 b5) bQuit) =
  InputState
    (replaceKeystate' zoomLevelDelta)
    ( DirectionalInput
        (replaceKeystate' b2)
        (replaceKeystate' b3)
        (replaceKeystate' b4)
        (replaceKeystate' b5)
    )
    (replaceKeystate' bQuit)
  where
    replaceKeystate' = replaceKeystate event
