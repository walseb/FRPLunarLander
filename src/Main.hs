{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL.Sprite
import           SDL
import           Linear
import           Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents

  -- Rendering
  rendererDrawColor renderer $= V4 0 0 255 255

  clear renderer
  present renderer

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  -- Also you have to have the laptop ready with arch today man
  sprite <- SDL.Sprite.load renderer "" V2(10 10)

  -- Exit
  unless (any (isKeyDown KeycodeQ) events)
    (appLoop renderer)

isKeyDown :: Keycode -> Event -> Bool
isKeyDown keycode event =
  case eventPayload event of
    -- If it's a keyboard event, check it further
    KeyboardEvent keyboardEvent -> keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode && keyboardEventKeyMotion keyboardEvent == Pressed
    _ -> False
