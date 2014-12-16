module Hunted.GameTypes where

import System.Random
import Graphics.Gloss.Data.ViewPort (ViewPort)

type Pos = (Float, Float)
data Player = Player { position :: Pos, movement :: Maybe PlayerMovement }
               deriving Show
data PlayerMovement = PlayerMovement { dir :: Direction, step :: Int }
               deriving Show
data Monster = Monster Pos MonsterStatus
               deriving Show

data MonsterStatus = Wander Direction Int
                   | Hunting Direction
               deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight
                 deriving (Show, Enum, Bounded)

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

data RenderState = RenderState Player Monster Bool ViewPort
data SoundState = SoundState (Maybe StatusChange) Bool Bool

data StatusChange = Danger | Safe
