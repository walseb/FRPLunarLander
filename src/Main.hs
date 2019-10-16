{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens

newtype Size = Size (V2 CInt)
  
data Object = Object {
                           _pos :: V2 CInt
                        ,  _size :: Size
                        , _alive :: Bool
                        }

makeLenses ''Object

data Resources = Resources {_objectSprite :: SDL.Sprite.Sprite }
makeLenses ''Resources
  
data GameState = GameState {
    _testObject :: Object
  , _enemies :: [Object]
  }
makeLenses ''GameState

spritePath :: FilePath
spritePath = "data/testSprite.png"

initialGame = GameState
              (Object (V2 0 0) (Size (V2 500 500)) True)
              [(Object (V2 600 600) (Size (V2 500 500)) True)]

render :: SDL.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer (Resources sprite) (GameState (Object a _ _) (x:_), exit) =
  do
    rendererDrawColor renderer $= V4 0 0 100 255
    clear renderer 
    SDL.Sprite.render sprite a
    SDL.Sprite.render sprite (x ^. pos)
    present renderer
    return exit

-- data InputEffect =
--   Exit Bool
--   | Move (InputState -> GameState -> GameState)
  
-- evalInputState :: GameState -> InputState -> GameState
-- evalInputState (GameState (Object obj)) inputState = 
--   GameState (Object
--               (obj + vectorizeMovement inputState))

collides :: (V2 CInt, Size) -> (V2 CInt, Size) -> Bool
collides
  -- Box1
  (V2 x0 y0, Size (V2 sizeX0 sizeY0))
  -- Box2
  (V2 x1 y1, Size (V2 sizeX1 sizeY1)) = 
    x0 < x1 + sizeX1 && 
    x0 + sizeX0 > x1 && 
    y0 < y1 + sizeY1 && 
    sizeY0 + y0 > y1

-- TODO this just checks against the first enemy
checkCollisions :: GameState -> GameState
checkCollisions state = (testObject . alive) `set` hasCollided $ state
  where hasCollided = collides (state ^. (testObject . pos), state ^. (testObject . size))
                               (state ^. enemies . to head . pos, state ^. enemies . to head . size)

objectSpeed :: V2 Double
objectSpeed = 100

movingObject :: V2 Double -> Y.SF (V2 CInt) (V2 CInt)
movingObject initialPos = proc move -> do
  -- Stop moving or something if a collision is detected
  p <- Y.integralFrom initialPos -< objectSpeed * fmap fromIntegral move 
  returnA -< (fmap floor p)

applyInputs :: Y.SF (GameState, InputState) GameState
applyInputs = proc input -> do
  objPos <- movingObject (V2 0 0) -< vectorizeMovement $ snd input

  let newState = (.~) (testObject . pos) objPos $ fst input

  returnA -< checkCollisions newState

update :: GameState -> Y.SF (Y.Event [SDL.Event]) (GameState, Bool)
update origGameState = proc events -> do
  newInputState <- accumHoldBy inputStateUpdate keybinds -< events

  gameState <- applyInputs -< (origGameState, newInputState)

  returnA -< (gameState,
                (_pressed . _quit) newInputState
              || (_alive . _testObject) gameState)

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

  reactimate (return Y.NoEvent) senseInput (\ _ -> render renderer resources) (update initialGame)
  
  destroyRenderer renderer
  destroyWindow window
