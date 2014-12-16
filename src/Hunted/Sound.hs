module Hunted.Sound (
  withSound
, loadSounds
, backgroundMusic
, playSounds
, Sounds(..)
) where

import Hunted.GameTypes

import Sound.ALUT hiding (Static)
import System.IO ( hPutStrLn, stderr )
import Data.List (intersperse)
import Control.Monad (when, unless)

data Sounds = Sounds { backgroundTune :: Source
                     , shriek :: Source
                     , bite :: Source }

-- convenience function to abstract the ALUT context
withSound = withProgNameAndArgs runALUT

-- sounds
-- music: https://www.freesound.org/people/Thirsk/sounds/121035/
-- shriek: https://www.freesound.org/people/dan2008ds/sounds/175169/
-- bite: https://www.freesound.org/people/dan2008ds/sounds/175169/
loadSounds :: IO Sounds
loadSounds = do
    music <- loadSound "sounds/oboe-loop.wav"
    shriek <- loadSound "sounds/shriek.wav"
    bite <- loadSound "sounds/bite.wav"
    sourceGain bite $= 0.5
    return $ Sounds music shriek bite

loadSound path = do
    buf <- createBuffer (File path)
    source <- genObjectName
    buffer source $= Just buf
    return source

backgroundMusic :: Source -> IO ()
backgroundMusic source = do
        loopingMode source $= Looping
        play [source]

paceToPitch Safe = 1
paceToPitch Danger = 2

playSounds :: Sounds -> SoundState -> IO ()
playSounds (Sounds music shriek bite) (SoundState mbPace endOfGame hunting) = do
  changeBackgroundMusic music mbPace 
  when endOfGame $ playSound shriek
  if hunting then playContinuousSound bite
             else stop [bite]

changeBackgroundMusic source (Just pace) = pitch source $= (paceToPitch pace)
changeBackgroundMusic source Nothing     = return ()

playContinuousSound source = do
        state <- get (sourceState source)
        unless (state == Playing) $ play [source]

playSound source = do
    play [source]
    -- Normally nothing should go wrong above, but one never knows...
    errs <- get alErrors
    unless (null errs) $ do
        hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
    return ()
