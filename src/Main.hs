{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import FRP.BearRiver as B
import FRP.Yampa as Y

import SDL
import qualified SDL.Sprite

import Data.String ( fromString )

import Foreign.C.Types ( CInt )

import Input
import Types ()

import Control.Concurrent ( newMVar, swapMVar )

import qualified Debug.Trace as Tr 

newtype Object = Object { _location :: V2 CInt }

data Resources = Resources {_objectSprite :: SDL.Sprite.Sprite }
data GameState = GameState {_testObject :: Object }

spritePath :: FilePath
spritePath = "data/testSprite.png"

render :: SDL.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite) (GameState (Object a), exit) =
  do
    rendererDrawColor renderer $= V4 0 0 100 255
    clear renderer 
    SDL.Sprite.render sprite a
    present renderer
    return exit

-- data InputEffect =
--   Exit Bool
--   | Move (InputState -> GameState -> GameState)
  
-- evalInputState :: GameState -> InputState -> GameState
-- evalInputState (GameState (Object obj)) inputState = 
--   GameState (Object
--               (obj + vectorizeMovement inputState))

objectSpeed :: V2 Double
objectSpeed = 100

movingObject :: V2 Double -> Y.SF (V2 CInt) Object
movingObject initialPos = proc move -> do
  p <- Y.integralFrom initialPos -< objectSpeed * fmap fromIntegral move 
  returnA -< (Object (fmap floor p))

update :: Y.SF (Y.Event [SDL.Event]) (GameState, Bool)
update = proc events -> do
  updateInputState <- accumHoldBy inputStateUpdate keybinds -< events

  gameState <- movingObject (V2 0 0) -< vectorizeMovement updateInputState
  -- gameState <- accumHoldBy evalInputState (GameState (Object (V2 0 0))) -< Y.Event updateInputState

  returnA -< (GameState gameState, (_pressed . _quit) updateInputState)
  
main :: IO ()
main = do
  initializeAll

  window     <- createWindow (fromString "My SDL Application") defaultWindow
  renderer   <- createRenderer window (-1) defaultRenderer
  spritetest <- SDL.Sprite.load renderer spritePath (V2 500 500)

  let resources = Resources spritetest

  lastInteraction <- newMVar =<< SDL.time

  let senseInput _canBlock = do
          currentTime <- SDL.time
          dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
          test <- Y.Event <$> SDL.pollEvents
          return (dt, Just test)

  reactimate (return Y.NoEvent) senseInput (\ _ -> render renderer resources) update
  
  destroyRenderer renderer
  destroyWindow window

