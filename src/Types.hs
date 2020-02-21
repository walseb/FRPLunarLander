{-# LANGUAGE TemplateHaskell #-}

module Types where

import Collision.Types
import Control.Lens
import qualified Data.Aeson.Tiled as Tiled
import Linear
import SDL as S
import SDL.Font (Font)

data ResourcesSelect
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
        land1 :: S.Texture,
        land2 :: S.Texture,
        land3 :: S.Texture,
        land4 :: S.Texture,
        terr1 :: S.Texture,
        terr2 :: S.Texture,
        terr3 :: S.Texture,
        terr4 :: S.Texture,
        terr5 :: S.Texture
      }

makeLenses ''Resources

data Object a
  = Object
      { _pos :: V2 a,
        _size :: V2 a,
        _rot :: a,
        _tex :: ResourcesSelect
      }
  deriving (Show)

makeLenses ''Object

data Terrain
  = Terrain
      { _coll :: [[Pt' Double]],
        _tObject :: Object Double
      }
  deriving (Show)

makeLenses ''Terrain

data Living
  = Living
      { _alive :: Bool,
        _lObject :: Object Double
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
        _score :: Int
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
