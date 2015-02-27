{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Control.Applicative ((<$>), (<*>))

import Testing.GameTypes
import Testing.Internals.Game

-- movePlayer always stays inside the playing field


prop_insideLimits move player = not $ (\p -> outsideOfLimits (worldWidth, worldHeight) p playerSize)
                                    $ position
                                    $ movePlayer playerSpeed (worldWidth, worldHeight) move Nothing (False, False, False, False) player
                                where types = (move :: (Bool, Bool, Bool, Bool), player :: Player)

instance Arbitrary Player where
  arbitrary = Player <$> ((,) <$> choose ((-worldWidth) `quot` 2 + playerSize `quot` 2, worldWidth `quot` 2 - playerSize `quot` 2)
                              <*> choose ((-worldHeight) `quot` 2 + playerSize `quot` 2, worldHeight `quot` 2 - playerSize `quot` 2))
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary PlayerMovement where
  arbitrary = PlayerMovement <$> arbitrary
                             <*> arbitrary
instance Arbitrary Direction where
  arbitrary = elements [WalkUp, WalkDown, WalkLeft, WalkRight]

instance Arbitrary WalkStage where
  arbitrary = elements [One, Two, Three, Four]

main :: IO ()
main = $defaultMainGenerator
