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
    (directionKey, directionKeySink) <- external (False, False, False, False)
    randomGenerator <- newStdGen
    glossState <- initState
    textures <- loadTextures
    withWindow width height "Game-Demo" $ \win -> do
      withSound $ \_ _ -> do
          sounds <- loadSounds
          backgroundMusic (backgroundTune sounds)
          network <- start $ hunted win (width,height) directionKey randomGenerator textures glossState sounds
          fix $ \loop -> do
               readInput win directionKeySink
               join network
               threadDelay 20000
               esc <- exitKeyPressed win
               unless esc loop
          exitSuccess
