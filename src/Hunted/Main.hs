import Hunted.Sound
import Hunted.Backend
import Hunted.Graphics
import Hunted.Game

import System.Exit ( exitSuccess )
import System.Random
import Control.Concurrent (threadDelay)
import Control.Monad (unless, join)
import Control.Monad.Fix (fix)
import FRP.Elerea.Simple as Elerea

width :: Int
width = 640

height :: Int
height = 480

main :: IO ()
main = do
    (directionKey, directionKeySink) <- unsafeExternal (False, False, False, False)
    (shootKey, shootKeySink) <- unsafeExternal (False, False, False, False)
    (windowSize,windowSizeSink) <- unsafeExternal (fromIntegral width, fromIntegral height)
    randomGenerator <- newStdGen
    glossState <- initState
    textures <- loadTextures
    withWindow width height windowSizeSink "hunted" $ \win -> do
      withSound $ \_ _ -> do
          sounds <- loadSounds
          backgroundMusic (backgroundTune sounds)
          network <- start $ hunted win windowSize directionKey shootKey randomGenerator textures glossState sounds
          fix $ \loop -> do
               readInput win directionKeySink shootKeySink
               join network
               threadDelay 20000
               esc <- exitKeyPressed win
               unless esc loop
          exitSuccess
