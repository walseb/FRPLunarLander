{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Input where

import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe
import Foreign.C.Types (CInt)
import SDL

data KeyState
  = KeyState
      { _key :: Keycode,
        _pressed :: Bool
      }
  deriving (Show)

makeLenses ''KeyState

data ZoomInput
  = ZoomInput
      { _zoomIn :: KeyState,
        _zoomOut :: KeyState
      }
  deriving (Show)

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
      { _zoom :: ZoomInput,
        _movement :: DirectionalInput,
        _quit :: KeyState
      }
  deriving (Show)

makeLenses ''InputState

keybinds = InputState
  {
    _zoom =
      ZoomInput
        (KeyState KeycodeM False)
        (KeyState KeycodeM False),
    _movement =
      DirectionalInput
        (KeyState KeycodeM False)
        (KeyState KeycodeT False)
        (KeyState KeycodeS False)
        (KeyState KeycodeN False),
    _quit = KeyState KeycodeQ False
  }

vectorizeMovement :: InputState -> V2 CInt
vectorizeMovement
  ( InputState
      _
      -- Movement keys
      ( DirectionalInput
          (KeyState _ a0)
          (KeyState _ a1)
          (KeyState _ a2)
          (KeyState _ a3)
        )
      _
    ) =
    V2
      -- Horizontal
      (boolToInt a2 - boolToInt a3)
      -- Vertical
      (boolToInt a0 - boolToInt a1)
    where
      boolToInt :: Bool -> CInt
      boolToInt a = if a then 0 else 1

translateEventPayload :: EventPayload -> Maybe KeyState
translateEventPayload (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) =
  Just (KeyState (keysymKeycode keysym) True)
translateEventPayload (KeyboardEvent (KeyboardEventData _ Released _ keysym)) =
  Just (KeyState (keysymKeycode keysym) False)
translateEventPayload _ = Nothing

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate =
  foldr $ \inpEv oldState ->
    fromMaybe oldState $
      (>=>)
        (translateEventPayload . eventPayload)
        (pure . updateKeyInInputState oldState)
        inpEv

-- So this is badly named, it just checks if the keys are identical then returns the old or then new state
replaceKeystate :: KeyState -> KeyState -> KeyState
replaceKeystate a0@(KeyState k0 _) a1@(KeyState k1 _) =
  if k0 == k1 then a0 else a1

updateKeyInInputState :: InputState -> KeyState -> InputState
updateKeyInInputState (InputState (ZoomInput b0 b1) (DirectionalInput b2 b3 b4 b5) bQuit) key =
  InputState
    ( ZoomInput
        (keyCheck b0)
        (keyCheck b1)
    )
    ( DirectionalInput
        (keyCheck b2)
        (keyCheck b3)
        (keyCheck b4)
        (keyCheck b5)
    )
    (keyCheck bQuit)
  where
    keyCheck = replaceKeystate key
