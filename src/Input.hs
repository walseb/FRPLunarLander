{-# LANGUAGE Arrows #-}

module Input
  ( -- Running an app
    inputStateUpdate
  , InputState
  , vectorizeMovement
  , keybinds
  , KeyState
  , _pressed
  , _quit
  )

where

import           FRP.BearRiver                 as B
import           FRP.Yampa                     as Y

import           SDL

import           Foreign.C.Types                ( CInt )

import           Control.Monad                  ( (>=>) )

import           Data.Maybe

data KeyState = KeyState {
    _key     :: Keycode,
    _pressed :: Bool
  }
  deriving Show

data InputState = InputState {
  _up :: KeyState
  , _down :: KeyState
  , _left :: KeyState
  , _right :: KeyState
  , _quit :: KeyState
            }
  deriving Show

keybinds = InputState { _up    = KeyState KeycodeM False
                      , _down  = KeyState KeycodeT False
                      , _left  = KeyState KeycodeS False
                      , _right = KeyState KeycodeN False
                      , _quit  = KeyState KeycodeQ False
                      }

vectorizeMovement :: InputState -> V2 CInt
vectorizeMovement (InputState
                    -- Movement keys
                              (KeyState _ a0) (KeyState _ a1) (KeyState _ a2) (KeyState _ a3) _)
  = V2
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
inputStateUpdate = foldr $ \inpEv oldState -> fromMaybe oldState $ (>=>)
  (translateEventPayload . eventPayload)
  (pure . updateKeyInInputState oldState)
  inpEv

updateKeyInInputState :: InputState -> KeyState -> InputState
updateKeyInInputState (InputState b0 b1 b2 b3 b4) c0 = InputState
  (replaceKeystate b0 c0)
  (replaceKeystate b1 c0)
  (replaceKeystate b2 c0)
  (replaceKeystate b3 c0)
  (replaceKeystate b4 c0)
