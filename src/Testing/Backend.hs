{-# LANGUAGE PackageImports #-}
module Testing.Backend (
  withWindow
, readInput
, exitKeyPressed
, swapBuffers
) where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))

--withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
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

readInput :: Window -> ((Bool, Bool, Bool, Bool) -> IO ()) -> ((Bool, Bool, Bool, Bool) -> IO ()) -> IO ()
readInput window directionKeySink shootKeySink = do
    pollEvents
    l <- keyIsPressed window Key'Left
    r <- keyIsPressed window Key'Right
    u <- keyIsPressed window Key'Up
    d <- keyIsPressed window Key'Down
    directionKeySink (l, r, u, d)
    shootKeySink =<< (,,,) <$> keyIsPressed window Key'A
                           <*> keyIsPressed window Key'D
                           <*> keyIsPressed window Key'W
                           <*> keyIsPressed window Key'S

exitKeyPressed :: Window -> IO Bool
exitKeyPressed window = keyIsPressed window Key'Escape
