{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Testing.GameTypes where

import System.Random
import Graphics.Gloss.Data.ViewPort
import Data.Monoid
import Data.Aeson
import GHC.Generics

type Pos = (Int, Int)
data Vec num = Vec num num

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

times :: Int -> Pos -> Pos
times a (x,y) = (a*x, a*y)
infixl 7 `times`

plus :: Pos -> Pos -> Pos
plus (a,b) (c,d) = getTupleSum $ (Sum a, Sum b) <> (Sum c, Sum d)
                   where getTupleSum (x, y) = (getSum x, getSum y)
infixl 6 `plus`

type Health = Int

data Player = Player { position :: Pos, movement :: Maybe PlayerMovement, shootDirection :: Maybe Direction }
               deriving (Show, Generic)
instance FromJSON Player
instance ToJSON Player

data PlayerMovement = PlayerMovement { dir :: Direction, step :: WalkStage }
               deriving (Show, Generic)
instance FromJSON PlayerMovement
instance ToJSON PlayerMovement

data WalkStage = One | Two | Three | Four
                 deriving (Show, Eq, Enum, Bounded, Generic)
instance FromJSON WalkStage
instance ToJSON WalkStage

circular :: (Eq x, Enum x, Bounded x) => x -> x
circular x = if x == maxBound then minBound else succ x

data Monster = Monster Pos MonsterStatus Health
               deriving Show

data MonsterStatus = Wander Direction Int
                   | Hunting Direction
               deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight
                 deriving (Show, Enum, Bounded, Eq, Generic)
instance FromJSON Direction
instance ToJSON Direction

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

data RenderState = RenderState { renderState_player :: Player
                               , renderState_monster :: [Monster]
                               , renderState_ending :: Maybe Ending
                               , renderState_viewport :: ViewPort
                               , renderState_bolts :: [Bolt]
                               , renderState_lives :: Int
                               , renderState_score :: Int
                               , renderState_animation :: Maybe Animation
                               , renderState_windowSize :: (Int, Int)
                               , renderState_levelCount :: LevelStatus }
                 | StartRenderState (Int, Int)
data SoundState = SoundState { mood :: (Maybe StatusChange)
                             , playerScreams :: Bool
                             , hunting :: Bool
                             , monsterDies :: Bool
                             , shoot :: Bool
                             , hit :: Bool }
                | StartSoundState

data GameState = GameState RenderState SoundState

data StatusChange = Danger | Safe

type Range = Int
data Bolt = Bolt Pos Direction Range Bool
              deriving Show

data Ending = Win | Lose
              deriving (Show, Eq)

data LevelStatus = Level Int
                   deriving (Show, Generic)
instance FromJSON LevelStatus
instance ToJSON LevelStatus

data GameStatus = Start | InGame
                  deriving (Show, Generic)
instance FromJSON GameStatus
instance ToJSON GameStatus

data Animation = DeathAnimation Float | NextLevelAnimation LevelStatus Float
                 deriving (Show, Generic)
instance FromJSON Animation
instance ToJSON Animation

data StartState = StartState { gameStatusSignal :: GameStatus
                             , levelCountSignal :: LevelStatus
                             , livesSignal :: Int
                             , scoreSignal :: Int
                             , playerSignal :: Player
                             , monsterPos :: Maybe [Pos]
                             , animationSignal :: Maybe Animation
                             , viewportTranslateSignal :: Pos }
                  deriving (Show, Generic)

instance FromJSON StartState
instance ToJSON StartState

data Command = LivesCommand Int
               deriving Show
