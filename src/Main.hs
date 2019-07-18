{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified SDL.Sprite
import           Control.Monad.Loops
import           SDL
import           Linear
import           Control.Monad                  ( unless )
import           Control.Lens

-- test
-- Also fix the gnus timer man

-- TODO
-- Use monad.loops
-- https://stackoverflow.com/questions/19285691/how-do-i-write-a-game-loop-in-haskell

-- http://amixtureofmusings.com/2016/05/19/associated-types-and-haskell/
-- And learn more about data vs class vs instance

-- We have a position
type Position = (Int, Int)

-- We have playerdata that only stores a position
data PlayerData = PlayerData { _pos :: Position }

data GameState = GameState PlayerData

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

-- Main
main :: IO ()
main = do
  initializeAll
  window   <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (GameState (PlayerData { _pos = (10, 10) }))

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
  events <- pollEvents

  -- Rendering
  rendererDrawColor renderer $= V4 0 0 255 255

  -- https://github.com/chrisdone/sdl2-sprite/blob/master/app/Main.hs
  sprite <- SDL.Sprite.load renderer "testSprite.png" (V2 10 10)

  clear renderer

  SDL.Sprite.render sprite (V2 10 10)

  present renderer

  -- Exit
  unless (any (isKeyDown KeycodeQ) events) (appLoop renderer state)

isKeyDown :: Keycode -> Event -> Bool
isKeyDown keycode event = case eventPayload event of
    -- If it's a keyboard event, check it further
  KeyboardEvent keyboardEvent ->
    keysymKeycode (keyboardEventKeysym keyboardEvent)
      == keycode
      && keyboardEventKeyMotion keyboardEvent
      == Pressed
  _ -> False
