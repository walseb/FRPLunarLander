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

makeLenses ''Object

data Terrain
  = Terrain
      { _collision :: [[Pt']],
        _tObject :: Object
      }

makeLenses ''Terrain

data Living
  = Living
      { _alive :: Bool,
        _lObject :: Object
      }

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

makeLenses ''PhysicalState

data CameraState
  = CameraState
      { _zoomLevel :: Double
      }

data GameState
  = GameState
      {
        _cameraState :: CameraState,
        _PhysicalState :: PhysicalState
      }

makeLenses ''GameState
