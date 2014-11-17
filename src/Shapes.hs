{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)

width  = 640
height = 480

main :: IO ()
main = do
    withWindow width height "Game-Demo" $ \win -> do
          loop win
          exitSuccess
loop window =  do
    threadDelay 20000
    pollEvents
    renderFrame window
    k <- keyIsPressed window Key'Escape
    unless k $ loop window

renderFrame window = do
     render (width, height) white $
       Pictures
                [ Line [(50, 50), ( 50,  100)]
                , Line [(50, 100), (65, 65)]
                , Line [(65, 65), (100, 65)]
                , lineLoop [(150, 150), (200, 150), (150, 200), (200, 200)]
                , Color red $ translate (-50) (-50) $ circle 50
                , Color green $ translate (-50) 50 $ circleSolid 50
                , Color violet $ translate 75 (-50) $ polygon [((-10), 10), ((-10), 70), (50, 50), (80, 90)]
                , Color magenta $ translate 25 0 $ rectangleSolid 20 20 ]
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
