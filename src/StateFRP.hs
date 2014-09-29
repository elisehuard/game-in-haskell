{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (Front)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Concurrent (threadDelay)
import Control.Monad (when, join)
import Control.Monad.Fix (fix)
import Control.Applicative ((<*>), (<$>))
import FRP.Elerea.Simple

type Pos = Vector2 GLdouble
data Player = Player Pos
data State = State { player :: Player }

initialState = State { player = Player (Vector2 200 200) }

initGL width height = do
  clearColor $= Color4 1 1 1 1
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

main :: IO ()
main = do
    let width  = 640
        height = 480
    (directionKey, directionKeySink) <- external (False, False, False, False)

    withWindow width height "Game-Demo" $ \win -> do
          initGL width height
          network <- start $ do
            player <- transfer initialState (\s dK -> movePlayer s dK 10) directionKey
            return $ renderFrame win <$> player
          fix $ \loop -> do
               readInput win directionKeySink
               join network
               threadDelay 20000
               esc <- keyIsPressed win Key'Escape
               when (not esc) loop
          exitWith ExitSuccess

readInput window directionKeySink = do
    pollEvents
    l <- keyIsPressed window Key'Left
    r <- keyIsPressed window Key'Right
    u <- keyIsPressed window Key'Up
    d <- keyIsPressed window Key'Down
    directionKeySink (l, r, u, d)

movePlayer (True, _, _, _) State { player = Player (Vector2 xpos ypos) } increment = State { player = Player (Vector2 (xpos - increment) ypos) }
movePlayer (_, True, _, _) State { player = Player (Vector2 xpos ypos) } increment = State { player = Player (Vector2 (xpos + increment) ypos) }
movePlayer (_, _, True, _) State { player = Player (Vector2 xpos ypos) } increment = State { player = Player (Vector2 xpos (ypos + increment)) }
movePlayer (_, _, _, True) State { player = Player (Vector2 xpos ypos) } increment = State { player = Player (Vector2 xpos (ypos - increment)) }
movePlayer (False, False, False, False) State { player = Player (Vector2 xpos ypos) } increment = State { player = Player (Vector2 xpos ypos) }

renderFrame window State { player = Player (Vector2 xpos ypos) } = do
   clear [ColorBuffer]
   color $ Color4 0 0 0 (1 :: GLfloat)
   let playerSize = (20 :: GLdouble)
   renderPrimitive Quads $ do
        vertex $ Vertex2 (xpos - playerSize/2) (ypos - playerSize/2)
        vertex $ Vertex2 (xpos + playerSize/2) (ypos - playerSize/2)
        vertex $ Vertex2 (xpos + playerSize/2) (ypos + playerSize/2)
        vertex $ Vertex2 (xpos - playerSize/2) (ypos + playerSize/2)
   color $ Color4 1 1 1 (1 :: GLfloat)
   flush
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
