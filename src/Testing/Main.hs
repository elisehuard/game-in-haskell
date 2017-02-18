import Testing.Sound
import Testing.Backend
import Testing.Graphics
import Testing.Game
import Testing.CommandLine

import System.Exit ( exitSuccess )
import System.Random
import Control.Concurrent (threadDelay)
import Control.Monad (unless, join, when)
import Control.Monad.Fix (fix)
import FRP.Elerea.Simple as Elerea
import Testing.GameTypes
import Options
import Control.Applicative ((<*>), pure)
import Control.Concurrent (forkIO, newEmptyMVar)
import Data.Aeson
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.ByteString.Lazy.Char8 as BC (lines)

width :: Int
width = 640

height :: Int
height = 480

data MainOptions = MainOptions {
  optStartFile :: Maybe String
, optInteractive :: Bool
, optLog :: Maybe String
} deriving Show

instance Options MainOptions where
  defineOptions = pure MainOptions
                <*> simpleOption "start-state" Nothing
                      "file containing start state"
                <*> simpleOption "interactive" False
                      "start an interactive session"
                <*> simpleOption "log" Nothing
                      "file containing input logs"

getStartState :: MainOptions -> IO StartState
getStartState opts = if (isJust (optStartFile opts))
                       then fmap (\mb -> fromMaybe defaultStart mb) $ fmap decode $ B.readFile (fromJust (optStartFile opts))
                       else return defaultStart

main :: IO ()
main = runCommand $ \opts _ -> do
    print opts
    startState <- getStartState opts
    commandVar <- newEmptyMVar
    when (optInteractive opts) $ do
      _ <- forkIO (interactiveCommandLine commandVar)
      return ()
    (snapshot, snapshotSink) <- unsafeExternal (0,False)
    (record, recordSink) <- unsafeExternal (0, False, False)
    (commands, commandSink) <- unsafeExternal Nothing
    (directionKey, directionKeySink) <- unsafeExternal (False, False, False, False)
    (shootKey, shootKeySink) <- unsafeExternal (False, False, False, False)
    (windowSize,windowSizeSink) <- unsafeExternal (fromIntegral width, fromIntegral height)
    randomGenerator <- newStdGen
    glossState <- initState
    textures <- loadTextures
    withWindow width height windowSizeSink "Game-Demo" $ \win -> do
      withSound $ \_ _ -> do
          sounds <- loadSounds
          backgroundMusic (backgroundTune sounds)
          network <- start $ hunted win
                                    windowSize
                                    directionKey
                                    shootKey
                                    randomGenerator
                                    textures
                                    glossState
                                    sounds
                                    startState
                                    snapshot
                                    record
                                    commands
          if (isJust (optLog opts))
          then do
            inputs <- externalInputs (fromJust (optLog opts))
            (flip mapM_) inputs $ \input -> do
                replayInput win input directionKeySink shootKeySink snapshotSink recordSink commandSink
                join network
                threadDelay 20000
          else
            fix $ \loop -> do
                readInput win directionKeySink shootKeySink snapshotSink recordSink commandSink commandVar
                join network
                threadDelay 20000
                esc <- exitKeyPressed win
                unless esc loop
          exitSuccess

externalInputs :: String
               -> IO [ExternalInput]
externalInputs file = fmap (map decodeOrThrow) $ fmap BC.lines $ B.readFile file
    where decodeOrThrow string = case (decode string :: Maybe ExternalInput) of
                                   Just x  -> x
                                   Nothing -> error $ "Log file contains line that can't be decoded: " ++ show string
