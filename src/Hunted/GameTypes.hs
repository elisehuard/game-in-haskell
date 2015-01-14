module Hunted.GameTypes where

import System.Random
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Data.Monoid

type Pos = (Float, Float)
data Vec num = Vec num num

dist :: Pos -> Pos -> Float
dist (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

times :: Float -> Pos -> Pos
times a (x,y) = (a*x, a*y)
infixl 7 `times`

plus :: Pos -> Pos -> Pos
plus (a,b) (c,d) = (a + c, b + d)
infixl 6 `plus`

type Health = Float

data Player = Player { position :: Pos, movement :: Maybe PlayerMovement }
               deriving Show
data PlayerMovement = PlayerMovement { dir :: Direction, step :: Int }
               deriving Show
data Monster = Monster Pos MonsterStatus Health
               deriving Show

data MonsterStatus = Wander Direction Int
                   | Hunting Direction
               deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight
                 deriving (Show, Enum, Bounded, Eq)

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

data RenderState = RenderState Player Monster (Maybe Ending) ViewPort [Bolt]
data SoundState = SoundState { mood :: (Maybe StatusChange)
                             , playerScreams :: Bool
                             , hunting :: Bool
                             , monsterDies :: Bool
                             , shoot :: Bool
                             , hit :: Bool }

data StatusChange = Danger | Safe

type Range = Int
data Bolt = Bolt Pos Direction Range Bool
              deriving Show

data Ending = Win | Lose
              deriving (Show, Eq)
