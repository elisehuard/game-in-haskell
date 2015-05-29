{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering as RS
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Control.Monad.State.Strict

width, height :: Int
width  = 640
height = 480

type Pos = Point
data Player = Player {position :: Pos}

initialPlayer :: Player
initialPlayer = Player (200,200)

playerSize :: Float
playerSize = 20

main :: IO ()
main = do
    glossState <- initState
    withWindow width height "Game-Demo" $ \win -> do
          _ <- runStateT (loop win glossState) initialPlayer
          exitSuccess

loop :: Window -> RS.State -> StateT Player IO ()
loop window glossState = do
            lift $ threadDelay 20000
            lift $ pollEvents
            k <- lift $keyIsPressed window Key'Escape
            l <- lift $keyIsPressed window Key'Left
            r <- lift $keyIsPressed window Key'Right
            u <- lift $keyIsPressed window Key'Up
            d <- lift $ keyIsPressed window Key'Down
            player <- get
            let newState = movePlayer (l,r,u,d) player 10
            put newState
            lift $ renderFrame newState window glossState
            unless k $ loop window glossState

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
movePlayer direction player increment
         | outsideOfLimits (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> Float -> Bool
outsideOfLimits (xmon, ymon) size = xmon > fromIntegral width/2 - size/2 ||
                                    xmon < (-(fromIntegral width)/2 + size/2) ||
                                    ymon > fromIntegral height/2 - size/2 ||
                                    ymon < (-(fromIntegral height)/2 + size/2)

move :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
move (True, _, _, _) (Player (xpos, ypos)) increment = Player ((xpos - increment), ypos)
move (_, True, _, _) (Player (xpos, ypos)) increment = Player ((xpos + increment), ypos)
move (_, _, True, _) (Player (xpos, ypos)) increment = Player (xpos, (ypos + increment))
move (_, _, _, True) (Player (xpos, ypos)) increment = Player (xpos, (ypos - increment))
move (False, False, False, False) (Player (xpos, ypos)) _ = Player (xpos, ypos)

renderFrame (Player (xpos, ypos)) window glossState = do
   displayPicture (width, height) white glossState 1.0 $ translate xpos ypos $ rectangleSolid playerSize playerSize
   swapBuffers window

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow windowWidth windowHeight title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow windowWidth windowHeight title Nothing Nothing
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
