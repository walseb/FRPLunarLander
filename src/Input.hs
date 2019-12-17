{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Input where

import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe
import Foreign.C.Types (CInt)
import SDL
import qualified Debug.Trace as Tr

data KeyState
  = KeyState
      { _key :: Keycode,
        _pressed :: Bool
      }
  deriving (Show)

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
      { _movement :: DirectionalInput,
        _quit :: KeyState
      }
  deriving (Show)

makeLenses ''InputState

keybinds = InputState
  { _movement =
      ( DirectionalInput
          (KeyState KeycodeM False)
          (KeyState KeycodeT False)
          (KeyState KeycodeS False)
          (KeyState KeycodeN False)
      ),
    _quit = KeyState KeycodeQ False
  }

vectorizeMovement :: InputState -> V2 CInt
vectorizeMovement
  ( InputState
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

replaceKeystate :: KeyState -> KeyState -> KeyState
replaceKeystate a0@(KeyState k0 _) a1@(KeyState k1 _) =
  if k0 == k1 then a1 else a0

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate = foldr $ \inpEv oldState ->
  fromMaybe oldState $
    (>=>)
      (translateEventPayload . eventPayload)
      (pure . updateKeyInInputState oldState)
      inpEv

updateKeyInInputState :: InputState -> KeyState -> InputState
updateKeyInInputState (InputState (DirectionalInput b0 b1 b2 b3) b4) c0 =
  InputState
    ( DirectionalInput
        (replaceKeystate b0 c0)
        (replaceKeystate b1 c0)
        (replaceKeystate b2 c0)
        (replaceKeystate b3 c0)
    )
    (replaceKeystate b4 c0)
