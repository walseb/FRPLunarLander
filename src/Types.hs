{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Linear
import qualified Sprite as SP
import YampaUtils.Types ()
import Collision.GJKInternal.Support

data Object
  = Object
      { _pos :: V2 Double,
        _size :: V2 Double,
        _rot :: Double
      }
  deriving(Show)

makeLenses ''Object

data Terrain
  = Terrain
      { _coll :: [[Pt']],
        _tObject :: Object
      }
  deriving(Show)


makeLenses ''Terrain

data Living
  = Living
      { _alive :: Bool,
        _lObject :: Object
      }
  deriving(Show)

makeLenses ''Living

data Resources
  = Resources
      { _objectSprite :: SP.Sprite,
        _objectSprite2 :: SP.Sprite,
        _scene :: SP.Sprite
      }

makeLenses ''Resources

data PhysicalState
  = PhysicalState
      { _player :: Living,
        _enemies :: [Living],
        _terrain :: [Terrain]
      }
  deriving(Show)

makeLenses ''PhysicalState

data CameraState
  = CameraState
      { _zoomLevel :: Int
      }
  deriving(Show)

data GameState
  = GameState
      {
        _cameraState :: CameraState,
        _physicalState :: PhysicalState
      }
  deriving(Show)

makeLenses ''GameState
