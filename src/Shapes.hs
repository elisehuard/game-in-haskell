{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (Front)
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)

initGL width height = do
  clearColor $= Color4 1 1 1 1
  ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1

main :: IO ()
main = do
    let width  = 640
        height = 480
    withWindow width height "Game-Demo" $ \win -> do
          initGL width height
          loop win
          exitSuccess
loop window =  do
    threadDelay 20000
    pollEvents
    renderFrame window
    k <- keyIsPressed window Key'Escape
    unless k $ loop window

renderFrame window = do
     clear[ColorBuffer]
     color $ Color4 0 0 0 (1 :: GLdouble)
     renderPrimitive Points $
        vertex $ Vertex2 20 (20 :: GLdouble)
     renderPrimitive Lines $ do
        vertex $ Vertex2 50 (50 :: GLdouble)
        vertex $ Vertex2 50 (100 :: GLdouble)
        vertex $ Vertex2 65 (65 :: GLdouble)
        vertex $ Vertex2 100 (65 :: GLdouble)
     renderPrimitive LineLoop $ do
        vertex $ Vertex2 150 (150 :: GLdouble)
        vertex $ Vertex2 200 (150 :: GLdouble)
        vertex $ Vertex2 150 (200 :: GLdouble)
        vertex $ Vertex2 200 (200 :: GLdouble)
     color $ Color4 1 0 0 (1 :: GLdouble)
     renderPrimitive TriangleStrip $ do
        vertex $ Vertex2 250 (250 :: GLdouble)
        vertex $ Vertex2 300 (250 :: GLdouble)
        vertex $ Vertex2 275 (300 :: GLdouble)
        vertex $ Vertex2 300 (300 :: GLdouble)
     color $ Color4 0 1 0 (1 :: GLdouble)
     renderPrimitive TriangleFan $ do
        vertex $ Vertex2 350 (350 :: GLdouble)
        vertex $ Vertex2 400 (350 :: GLdouble)
        vertex $ Vertex2 375 (400 :: GLdouble)
        vertex $ Vertex2 300 (400 :: GLdouble)
     color $ Color4 1 1 0.2 (1 :: GLdouble)
     let poly  = 24
         ang p = p * 2 * pi / poly
         r = 50
         pos   = map (\p -> (400 + cos(ang p)*r, 100 + sin(ang p)*r)) [1,2..poly]
     renderPrimitive Polygon $
        mapM_ (\(x,y) -> vertex $ Vertex2 x (y :: GLdouble)) pos
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
