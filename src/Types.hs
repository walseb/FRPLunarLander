{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import SDL as S
import SDL.Font (Font)
import FRPEngine.Types

data Resources
  = Resources
      { _font :: Font,
        _hidden :: S.Texture,
        _objectSprite :: S.Texture,
        _objectSprite2 :: S.Texture,
        _sceneSprite :: S.Texture,
        _sceneDangerousSprite :: S.Texture,
        _land1 :: S.Texture,
        _land2 :: S.Texture,
        _land3 :: S.Texture,
        _land4 :: S.Texture,
        _terr1 :: S.Texture,
        _terr2 :: S.Texture,
        _terr3 :: S.Texture,
        _terr4 :: S.Texture,
        _terr5 :: S.Texture
      }

makeLenses ''Resources

data SpriteSelect
  = Sfont
  | SHidden
  | SobjectSprite
  | SobjectSprite2
  | SsceneSprite
  | SsceneDangerousSprite
  | Sland1
  | Sland2
  | Sland3
  | Sland4
  | Sterr1
  | Sterr2
  | Sterr3
  | Sterr4
  | Sterr5
  deriving (Show)

getSprite :: Object a SpriteSelect -> Resources -> S.Texture
getSprite obj =
  case (obj ^. spr) of
    SHidden -> _hidden
    SobjectSprite -> _objectSprite
    SobjectSprite2 -> _objectSprite2
    SsceneSprite -> _sceneSprite
    SsceneDangerousSprite -> _sceneDangerousSprite
    Sland1 -> _land1
    Sland2 -> _land2
    Sland3 -> _land3
    Sland4 -> _land4
    Sterr1 -> _terr1
    Sterr2 -> _terr2
    Sterr3 -> _terr3
    Sterr4 -> _terr4
    Sterr5 -> _terr5

data Living
  = Living
      { _alive :: Bool,
        _lObj :: Object Double SpriteSelect
      }
  deriving (Show)

makeLenses ''Living

data LandingSpot
  = LandingSpot
      { _pointValue :: Int,
        _lCollObj :: CollObj SpriteSelect
      }
  deriving (Show)

makeLenses ''LandingSpot

data Scene
  = Scene
      { _sCollObj :: [CollObj SpriteSelect],
        _landingSpots :: [LandingSpot]
      }
  deriving (Show)

makeLenses ''Scene

data Player
  = Player
      { _pLiving :: Living,
        _score :: Int,
        _fuel :: Double
      }
  deriving (Show)

makeLenses ''Player

data MovingState
  = MovingState
      { _player :: Player }
  deriving (Show)

makeLenses ''MovingState

data PhysicalState
  = PhysicalState
      { _movingState :: MovingState,
        _scene :: Scene
      }
  deriving (Show)

makeLenses ''PhysicalState

data CameraState
  = CameraState
      { _zoomLevel :: Int
      }
  deriving (Show)

data GameState
  = GameState
      { _cameraState :: CameraState,
        _physicalState :: PhysicalState
      }
  deriving (Show)

makeLenses ''GameState
