{-# LANGUAGE PackageImports #-}
module Testing.Backend (
  withWindow
, readInput
, replayInput
, exitKeyPressed
, swapBuffers
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (MVar, tryTakeMVar)
import Data.Maybe (isJust)
import Testing.GameTypes
import Data.Time.Clock.POSIX

withWindow :: Int
            -> Int
            -> ((Int, Int) -> IO ())
            -> String
            -> (Window -> IO ())
            -> IO ()
withWindow width height windowSizeSink title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              setWindowSizeCallback win $ Just $ resize windowSizeSink
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

resize :: ((Int, Int) -> IO()) -> Window -> Int -> Int -> IO()
resize windowSizeSink _ w h = windowSizeSink (w, h)

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False

readInput :: Window
          -> ((Bool, Bool, Bool, Bool) -> IO ())
          -> ((Bool, Bool, Bool, Bool) -> IO ())
          -> ((Int, Bool) -> IO ())
          -> ((Int, Bool, Bool) -> IO ())
          -> (Maybe Command -> IO ())
          -> MVar Command
          -> IO ()
readInput window directionKeySink shootKeySink snapshotSink recordSink commandSink commandVar = do
    pollEvents
    directionKeySink =<< (,,,) <$> keyIsPressed window Key'Left
                               <*> keyIsPressed window Key'Right
                               <*> keyIsPressed window Key'Up
                               <*> keyIsPressed window Key'Down
    shootKeySink =<< (,,,) <$> keyIsPressed window Key'A
                           <*> keyIsPressed window Key'D
                           <*> keyIsPressed window Key'W
                           <*> keyIsPressed window Key'S

    startRecording <- keyIsPressed window Key'R
    endRecording <- keyIsPressed window Key'E
    timestamp <- round `fmap` getPOSIXTime

    snapshotting <- keyIsPressed window Key'T
    snapshotSink (timestamp, snapshotting)
    recordSink (timestamp, startRecording, endRecording)

    mbCommand <- tryTakeMVar commandVar
    when (isJust mbCommand) $ print mbCommand
    commandSink mbCommand

replayInput :: Window
            -> ExternalInput
            -> ((Bool, Bool, Bool, Bool) -> IO ())
            -> ((Bool, Bool, Bool, Bool) -> IO ())
            -> ((Int, Bool) -> IO ())
            -> ((Int, Bool, Bool) -> IO ())
            -> (Maybe Command -> IO ())
            -> IO ()
replayInput win
            (ExternalInput directionKey shootKey)
            directionKeySink
            shootKeySink
            snapshotSink
            recordSink
            commandSink = do
  pollEvents
  directionKeySink directionKey
  shootKeySink shootKey
  snapshotSink (0, False)
  recordSink (0, False, False)
  commandSink Nothing

exitKeyPressed :: Window -> IO Bool
exitKeyPressed window = keyIsPressed window Key'Escape
