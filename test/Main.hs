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

main = defaultMain hunitSuite

hunitSuite = [ testGroup "monsterHits" [ testCase "no bolts" caseNoBolts
                                       , testCase "no bolts close" caseNoHits
                                       , testCase "one bolt close out of two" caseOneHit
                                       , testCase "no counting if already hit" caseAlreadyHit ] ]

caseNoBolts = 0 @=? monsterHits []
                                (Monster (0,0) (Wander WalkUp 2) 2)

caseNoHits = 0 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                               , Bolt (200, 200) WalkDown 3 False ]
                               (Monster (0,0) (Hunting WalkUp) 2)

caseOneHit = 1 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                               , Bolt (5,5) WalkDown 3 False ]
                               (Monster (0,0) (Hunting WalkUp) 2)

caseAlreadyHit = 0 @=? monsterHits [ Bolt (100, 100) WalkUp 4 False
                                   , Bolt (5,5) WalkDown 3 True ]
                                   (Monster (0,0) (Hunting WalkUp) 2)
