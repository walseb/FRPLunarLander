{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Linear
import SDL.Font (Font)
import SDL as S
import Collision.Types

data Object a
  = Object
      { _pos :: V2 a,
        _size :: V2 a,
        _rot :: a
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

data Resources
  = Resources
      {
        _font :: Font,
        _objectSprite :: S.Texture,
        _objectSprite2 :: S.Texture,
        _sceneSprite :: S.Texture,
        _sceneDangerousSprite :: S.Texture
      }

makeLenses ''Resources

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
