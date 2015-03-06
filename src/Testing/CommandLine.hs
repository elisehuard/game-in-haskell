module Testing.CommandLine (
  interactiveCommandLine
) where

import Testing.GameTypes
import Testing.Internals.CommandParser
import System.Console.Haskeline
import Control.Concurrent (MVar, putMVar)
import Control.Monad.IO.Class
import Data.Text (pack)

interactiveCommandLine :: MVar Command -> IO ()
interactiveCommandLine commandVar = do
  runInputT defaultSettings loop
  putStrLn "interactive command line loop ended"
    where
      loop :: InputT IO ()
      loop = do
        minput <- getInputLine "% "
        case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just input -> do case parseCommand (pack input) of
                             Right command -> liftIO $ putMVar commandVar command
                             Left error    -> outputStrLn $ error
                           loop
