{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Linear
import qualified Sprite as SP
import YampaUtils.Types ()

data Object
  = Object
      { _pos :: V2 Double,
        _size :: V2 Double,
        _rot :: Double,
        _alive :: Bool
      }

makeLenses ''Object

data Resources
  = Resources
      { _objectSprite :: SP.Sprite,
        _objectSprite2 :: SP.Sprite,
        _scene :: SP.Sprite
      }

makeLenses ''Resources

data Objects
  = Objects
      { _player :: Object,
        _enemies :: [Object]
      }

makeLenses ''Objects

data CameraState
  = CameraState
      { _zoomLevel :: Double
      }

data GameState
  = GameState
      {
        _cameraState :: CameraState,
        _objects :: Objects
      }

makeLenses ''GameState
