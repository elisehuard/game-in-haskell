{-# LANGUAGE PackageImports, RecursiveDo #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, join)
import Control.Monad.Fix (fix)
import Control.Applicative ((<*>), (<$>))
import FRP.Elerea.Simple
import Foreign.C.Types (CDouble(..))
import System.Random

type Pos = Point
data Player = Player { position :: Pos }
type Hunting = Bool
data Monster = Monster Pos MonsterStatus
               deriving Show

data MonsterStatus = Wander Direction Int
                   | Hunting
               deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight
                 deriving (Show, Enum, Bounded)

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

initialPlayer = Player (0, 0)
initialMonster = Monster (200, 200) (Wander WalkUp wanderDist)
width = 640
height = 480
playerSize = 20
monsterSize = 20
monsterSpeed = 5

main :: IO ()
main = do
    (directionKey, directionKeySink) <- external (False, False, False, False)
    randomGenerator <- newStdGen
    playerTexture <- loadBMP "images/alien.bmp"
    backgroundTexture <- loadBMP "images/background-1.bmp"
    monsterWalkingTexture <- loadBMP "images/monster-walking.bmp"
    monsterHuntingTexture <- loadBMP "images/monster-hunting.bmp"
    withWindow width height "Game-Demo" $ \win -> do
          network <- start $ hunted win directionKey randomGenerator [playerTexture, monsterWalkingTexture, monsterHuntingTexture, backgroundTexture]
          fix $ \loop -> do
               readInput win directionKeySink
               join network
               threadDelay 20000
               esc <- keyIsPressed win Key'Escape
               unless esc loop
          exitSuccess

hunted win directionKey randomGenerator textures = mdo
    player <- transfer2 initialPlayer (\p dead dK -> movePlayer p dK dead 10) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer3 initialMonster wanderOrHunt player randomNumber gameOver'
    gameOver <- memo (playerEaten <$> player <*> monster)
    gameOver' <- delay False gameOver
    return $ renderFrame win textures <$> player <*> monster <*> gameOver
    where playerEaten player monster = distance player monster < (10^2  :: Float)
          nextRandom (a, g) = random g

readInput window directionKeySink = do
    pollEvents
    l <- keyIsPressed window Key'Left
    r <- keyIsPressed window Key'Right
    u <- keyIsPressed window Key'Up
    d <- keyIsPressed window Key'Down
    directionKeySink (l, r, u, d)

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Bool -> Float -> Player
movePlayer _ player True _ = player
movePlayer direction player@(Player (xpos, ypos)) False increment
         | outsideOfLimits (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> Float -> Bool
outsideOfLimits (xmon, ymon) size = xmon > fromIntegral width/2 - size/2 ||
                                    xmon < (-(fromIntegral width)/2 + size/2) ||
                                    ymon > fromIntegral height/2 - size/2 ||
                                    ymon < (-(fromIntegral height)/2 + size/2)

move (True, _, _, _) (Player (xpos, ypos)) increment = Player ((xpos - increment), ypos)
move (_, True, _, _) (Player (xpos, ypos)) increment = Player ((xpos + increment), ypos)
move (_, _, True, _) (Player (xpos, ypos)) increment = Player (xpos, (ypos + increment))
move (_, _, _, True) (Player (xpos, ypos)) increment = Player (xpos, (ypos - increment))
move (False, False, False, False) (Player (xpos, ypos)) _ = Player (xpos, ypos)

wanderDist = 40
huntingDist = 100

wanderOrHunt _ _ True monster = monster
wanderOrHunt player (r, _) False monster = if close player monster
                                                then hunt player monster
                                                else wander r monster

close player monster = distance player monster < huntingDist^2

distance (Player (xpos, ypos)) (Monster (xmon, ymon) _) = (xpos - xmon)^2 + (ypos - ymon)^2

-- if player is upper left quadrant, diagonal left
-- means xpos > xmon and ypos > ymon
hunt :: Player -> Monster -> Monster
hunt (Player (xpos, ypos)) (Monster (xmon, ymon) _) = Monster ((xmon + (signum (xpos - xmon))*monsterSpeed), (ymon + (signum (ypos - ymon))*monsterSpeed)) Hunting

-- turn in random direction
wander :: Direction -> Monster -> Monster
wander r (Monster (xmon, ymon) (Wander _ 0)) = Monster (xmon, ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) Hunting) = Monster (xmon, ymon) (Wander r wanderDist)
-- go straight
wander _ (Monster (xmon, ymon) (Wander direction n)) = do
                   let currentDirection = continueDirection direction (outsideOfLimits (xmon, ymon) monsterSize)
                   Monster
                       (stepInCurrentDirection currentDirection (xmon, ymon) monsterSpeed)
                       (Wander currentDirection (n-1))

continueDirection :: Direction -> Bool -> Direction
continueDirection WalkUp True = WalkDown
continueDirection WalkDown True = WalkUp
continueDirection WalkLeft True = WalkRight
continueDirection WalkRight True = WalkLeft
continueDirection direction False = direction

stepInCurrentDirection WalkUp (xpos, ypos)    speed = (xpos, ypos + speed)
stepInCurrentDirection WalkDown (xpos, ypos)  speed = (xpos, ypos - speed)
stepInCurrentDirection WalkLeft (xpos, ypos)  speed = (xpos - speed, ypos)
stepInCurrentDirection WalkRight (xpos, ypos) speed = (xpos + speed, ypos)

renderFrame :: Window -> [Picture] -> Player -> Monster -> Bool -> IO ()
renderFrame window [playerTexture, monsterWalkingTexture, monsterHuntingTexture, backgroundTexture] (Player (xpos, ypos)) (Monster (xmon, ymon) status) gameOver = do
   render (width, height) white $ 
     Pictures $ gameOngoing gameOver
                             [backgroundTexture,
                              renderPlayer xpos ypos playerTexture,
                              renderMonster status xmon ymon (monsterWalkingTexture, monsterHuntingTexture)]
   swapBuffers window

renderPlayer :: Float -> Float -> Picture -> Picture
--renderPlayer xpos ypos texture = translate xpos ypos $ scale 0.2 0.2 $ texture
renderPlayer xpos ypos texture = translate xpos ypos $ texture

renderMonster :: MonsterStatus -> Float -> Float -> (Picture, Picture) -> Picture
renderMonster Hunting xpos ypos (_, monsterHuntingTexture) = translate xpos ypos $ monsterHuntingTexture
renderMonster (Wander _ _) xpos ypos (monsterWalkingTexture, _) = translate xpos ypos $ monsterWalkingTexture

-- adds gameover text if appropriate
gameOngoing :: Bool -> [Picture] -> [Picture]
gameOngoing gameOver pics = if gameOver then pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
                                        else pics

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
