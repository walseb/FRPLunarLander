{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import SDL as S
import SDL.Font (Font)
import FRPEngine.Types
import Data.Aeson
import GHC.Generics

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
  deriving (Generic, Show)

getSprite :: Obj a SpriteSelect -> Resources -> S.Texture
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

data LandingSpot a
  = LandingSpot
      { _pointValue :: Int,
        _lCollObj :: CollObj a SpriteSelect
      }
  deriving (Generic, Show)

makeLenses ''LandingSpot

data Scene a
  = Scene
      { _terrain :: [CollObj a SpriteSelect],
        _landingSpots :: [LandingSpot a]
      }
  deriving (Generic, Show)

makeLenses ''Scene

data Player a
  = Player
      { _alive :: Bool,
        _pCollObj :: CollObj a SpriteSelect,
        _score :: Int,
        _fuel :: a
      }
  deriving (Generic, Show)

makeLenses ''Player

data PhysicalState a
  = PhysicalState
      {
        _player :: Player a,
        _scene :: Scene a
      }
  deriving (Generic, Show)

makeLenses ''PhysicalState

data CameraState a
  = CameraState
      { _zoomLevel :: a
      }
  deriving (Generic, Show)

data GameState a
  = GameState
      { _cameraState :: CameraState a,
        _physicalState :: PhysicalState a
      }
  deriving (Generic, Show)

makeLenses ''GameState

instance (FromJSON a) => FromJSON (GameState a)
instance (ToJSON a) => ToJSON (GameState a)

instance (FromJSON a) => FromJSON (CameraState a)
instance (ToJSON a) => ToJSON (CameraState a)

instance (FromJSON a) => FromJSON (Player a)
instance (ToJSON a) => ToJSON (Player a)

instance (FromJSON a) => FromJSON (LandingSpot a)
instance (ToJSON a) => ToJSON (LandingSpot a)

instance (FromJSON a) => FromJSON (Scene a)
instance (ToJSON a) => ToJSON (Scene a)

instance (FromJSON a) => FromJSON (PhysicalState a)
instance (ToJSON a) => ToJSON (PhysicalState a)

instance ToJSON SpriteSelect
instance FromJSON SpriteSelect
