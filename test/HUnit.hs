{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit hiding (test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework

-- actual code
import Testing.Internals.Game
import Testing.GameTypes
import Testing.Internals.CommandParser

main = defaultMain [$testGroupGenerator]

case_NoBolts = 0 @=? monsterHits []
                                (Monster (0,0) (Wander WalkUp 2) 2)

case_NoHits = 0 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                               , Bolt (200, 200) WalkDown 3 False ]
                               (Monster (0,0) (Hunting WalkUp) 2)

case_OneHit = 1 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                               , Bolt (5,5) WalkDown 3 False ]
                               (Monster (0,0) (Hunting WalkUp) 2)

case_AlreadyHit = 0 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                                   , Bolt (5,5) WalkDown 3 True ]
                                   (Monster (0,0) (Hunting WalkUp) 2)

case_LivesCommand = Right (LivesCommand 5) @=? parseCommand "lives = 5"

case_playerPosCommand = Right (PlayerPosCommand (100,100)) @=? parseCommand "playerPos = (100, 100)"
case_playerPosCommand_neg = Right (PlayerPosCommand ((-100),(-100))) @=? parseCommand "playerPos = (-100, -100)"
