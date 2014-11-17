{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)

width  = 640
height = 480

type Pos = Point
data Player = Player Pos

initialPlayer = Player (200,200)

main :: IO ()
main = do
    withWindow width height "Game-Demo" $ \win -> do
          loop win initialPlayer
          exitSuccess
    where loop window state =  do
            threadDelay 20000
            pollEvents
            k <- keyIsPressed window Key'Escape
            l <- keyIsPressed window Key'Left
            r <- keyIsPressed window Key'Right
            u <- keyIsPressed window Key'Up
            d <- keyIsPressed window Key'Down
            let newState = movePlayer (l,r,u,d) state 10
            renderFrame newState window
            unless k $ loop window newState

movePlayer (True, _, _, _) (Player (xpos, ypos)) increment = Player ((xpos - increment), ypos)
movePlayer (_, True, _, _) (Player (xpos, ypos)) increment = Player ((xpos + increment), ypos)
movePlayer (_, _, True, _) (Player (xpos, ypos)) increment = Player (xpos, (ypos + increment))
movePlayer (_, _, _, True) (Player (xpos, ypos)) increment = Player (xpos, (ypos - increment))
movePlayer (False, False, False, False) (Player (xpos, ypos)) _ = Player (xpos, ypos)

renderFrame (Player (xpos, ypos)) window = do
   let playerSize = 20
   render (width, height) white $ translate xpos ypos $ rectangleSolid playerSize playerSize
   swapBuffers window

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
