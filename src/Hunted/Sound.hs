{-# LANGUAGE ScopedTypeVariables #-}
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
-- import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Class ()
import Control.Applicative ((<$>), (<*>))

data Sounds = Sounds { backgroundTune :: Source
                     , shriek :: Source
                     , bite :: Source
                     , groan :: Source
                     , twang :: Source
                     , thump :: Source }

-- convenience function to abstract the ALUT context
withSound :: forall a. Runner IO a
withSound = withProgNameAndArgs runALUT

-- sounds
-- music: https://www.freesound.org/people/Thirsk/sounds/121035/
-- shriek: https://www.freesound.org/people/dan2008ds/sounds/175169/
-- bite: https://www.freesound.org/people/dan2008ds/sounds/175169/
-- twang https://www.freesound.org/people/cubic.archon/sounds/44192/
-- thump https://www.freesound.org/people/fons/sounds/101362/
-- groan https://www.freesound.org/people/dag451/sounds/118336/

loadSounds :: IO Sounds
loadSounds = do
    biteSource <- loadSound "sounds/bite.wav"
    sourceGain biteSource $= 0.5
    Sounds <$> loadSound "sounds/oboe-loop.wav"
           <*> loadSound "sounds/shriek.wav"
           <*> return biteSource
           <*> loadSound "sounds/groan.wav"
           <*> loadSound "sounds/twang.wav"
           <*> loadSound "sounds/thump.wav"

loadSound :: FilePath -> IO Source
loadSound path = do
    buf <- createBuffer (File path)
    source <- genObjectName
    buffer source $= Just buf
    return source

backgroundMusic :: Source -> IO ()
backgroundMusic source = do
        loopingMode source $= Looping
        play [source]

-- ALUT internal float format
paceToPitch :: StatusChange -> ALfloat
paceToPitch Safe = 1
paceToPitch Danger = 2

playSounds :: Sounds -> SoundState -> IO ()
playSounds _      StartSoundState = return ()
playSounds sounds soundState = do
  changeBackgroundMusic (backgroundTune sounds) (mood soundState)
  when (playerScreams soundState) $ playSound (shriek sounds)
  when (monsterDies soundState) $ playSound (groan sounds)
  when (shoot soundState) $ playSound (twang sounds)
  when (hit soundState) $ playSound (thump sounds)
  if (hunting soundState) then playContinuousSound (bite sounds)
                          else stop [bite sounds]

changeBackgroundMusic :: Source -> Maybe StatusChange -> IO ()
changeBackgroundMusic source (Just pace) = pitch source $= (paceToPitch pace)
changeBackgroundMusic _       Nothing     = return ()

playContinuousSound :: Source -> IO ()
playContinuousSound source = do
        state <- get (sourceState source)
        unless (state == Playing) $ play [source]

playSound :: Source -> IO ()
playSound source = do
    play [source]
    -- Normally nothing should go wrong above, but one never knows...
    errs <- get alErrors
    unless (null errs) $ do
        hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
    return ()
