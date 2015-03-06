module Testing.CommandLine (
  interactiveCommandLine
) where

import Testing.GameTypes
import System.Console.Haskeline
import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad.IO.Class

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
          Just input -> do outputStrLn $ "input received : " ++ input
                           liftIO $ putMVar commandVar $ LivesCommand 1
                           loop
