{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Input where

import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe
import SDL

data KeyState
  = ButtonState {_key :: Keycode, _pressed :: Bool}
  | ScrollState {_zoomLevel :: Double}
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
      (ScrollState 20),
    _movement =
      DirectionalInput
        (ButtonState KeycodeM False)
        (ButtonState KeycodeT False)
        (ButtonState KeycodeS False)
        (ButtonState KeycodeN False),
    _quit = ButtonState KeycodeQ False
  }

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) = Just (ButtonState (keysymKeycode keysym) True)
translateEventPayload (KeyboardEvent (KeyboardEventData _ Released _ keysym)) = Just (ButtonState (keysymKeycode keysym) False)
translateEventPayload (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal)) = Just (ScrollState (fromIntegral (scrollDist ^. _y)))
translateEventPayload (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped)) = Just (ScrollState (- (fromIntegral (scrollDist ^. _y))))
translateEventPayload _ = Nothing

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate =
  foldr $ \inputEvent oldState ->
    fromMaybe oldState $
      (>=>)
        (translateEventPayload . eventPayload)
        (pure . updateKeyInInputState oldState)
        inputEvent

-- So this is badly named, it just checks if the keys are identical then returns the old or then new state
-- First key state is the initial key. The second is the updated/new key
replaceKeystate :: KeyState -> KeyState -> KeyState
replaceKeystate a0@(ButtonState k0 _) a1@(ButtonState k1 _) = if k0 == k1 then a0 else a1
replaceKeystate (ScrollState newZoom) (ScrollState oldZoom) =
  if (oldZoom + newZoom') > 0
    then ScrollState (oldZoom + newZoom')
    else ScrollState oldZoom
  where newZoom' = -newZoom
-- If keys aren't matched don't do anything
replaceKeystate _ a = a

updateKeyInInputState :: InputState -> KeyState -> InputState
updateKeyInInputState (InputState zoomLevel (DirectionalInput b2 b3 b4 b5) bQuit) key =
  InputState
    (replaceKeystate' zoomLevel)
    ( DirectionalInput
        (replaceKeystate' b2)
        (replaceKeystate' b3)
        (replaceKeystate' b4)
        (replaceKeystate' b5)
    )
    (replaceKeystate' bQuit)
  where
    replaceKeystate' = replaceKeystate key
