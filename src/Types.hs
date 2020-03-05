{-# LANGUAGE TemplateHaskell #-}

module Types where

import Collision.Types
import Control.Lens
import qualified Data.Aeson.Tiled as Tiled
import Linear
import SDL as S
import SDL.Font (Font)

data SpriteSelect
  = Sfont
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

data Resources
  = Resources
      { _font :: Font,
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

getSprite :: SpriteSelect -> Resources -> S.Texture
getSprite s =
  case s of
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

data Object a
  = Object
      { _pos :: V2 a,
        _size :: V2 a,
        _rot :: a,
        _spr :: SpriteSelect
      }
  deriving (Show)

makeLenses ''Object

data Terrain
  = Terrain
      { _coll :: [[Pt' Double]],
        _tObj :: Object Double
      }
  deriving (Show)

makeLenses ''Terrain

data Living
  = Living
      { _alive :: Bool,
        _lObj :: Object Double
      }
  deriving (Show)

makeLenses ''Living

data LandingSpot
  = LandingSpot
      { _pointValue :: Int,
        _lTerrain :: Terrain
      }
  deriving (Show)

makeLenses ''LandingSpot

data Scene
  = Scene
      { _sTerrain :: [Terrain],
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
      { _player :: Player,
        _enemies :: [Living]
      }
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
