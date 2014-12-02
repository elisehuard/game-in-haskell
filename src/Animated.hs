{-# LANGUAGE PackageImports, RecursiveDo #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, join)
import Control.Monad.Fix (fix)
import Control.Applicative ((<*>), (<$>))
import FRP.Elerea.Simple
import Foreign.C.Types (CDouble(..))
import System.Random
import Debug.Trace

type Pos = Point
data Player = Player { position :: Pos, dir :: Maybe Direction }
               deriving Show
data Monster = Monster Pos MonsterStatus
               deriving Show

data MonsterStatus = Wander Direction Int
                   | Hunting Direction
               deriving Show
data Direction = WalkUp | WalkDown | WalkLeft | WalkRight
                 deriving (Show, Enum, Bounded)

instance Random Direction where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                       (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

data TextureSet = TextureSet { front :: Picture, back :: Picture, left :: Picture, right :: Picture }
data Textures = Textures { background :: Picture
                         , player :: TextureSet
                         , monsterWalking :: TextureSet
                         , monsterHunting :: TextureSet }

initialPlayer = Player (0, 0) Nothing
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
    glossState <- initState
    textures <- loadTextures
    withWindow width height "Game-Demo" $ \win -> do
          network <- start $ hunted win directionKey randomGenerator textures glossState
          fix $ \loop -> do
               readInput win directionKeySink
               join network
               threadDelay 20000
               esc <- keyIsPressed win Key'Escape
               unless esc loop
          exitSuccess

loadTextures :: IO Textures
loadTextures = do
    playerTextureSet <- TextureSet <$> loadBMP "images/knight-front.bmp"
                                   <*> loadBMP "images/knight-back.bmp"
                                   <*> loadBMP "images/knight-left.bmp"
                                   <*> loadBMP "images/knight-right.bmp"
    monsterWalkingSet <- TextureSet <$> loadBMP "images/monster-walking-front.bmp"
                                    <*> loadBMP "images/monster-walking-back.bmp"
                                    <*> loadBMP "images/monster-walking-left.bmp"
                                    <*> loadBMP "images/monster-walking-right.bmp"
    -- moves diagonally, so only 2 textures needed technically
    monsterHuntingSet <- TextureSet <$> loadBMP "images/monster-hunting-left.bmp"
                                    <*> loadBMP "images/monster-hunting-right.bmp"
                                    <*> loadBMP "images/monster-hunting-left.bmp"
                                    <*> loadBMP "images/monster-hunting-right.bmp"
    backgroundTexture <- loadBMP "images/background-1.bmp"
    return Textures { background = backgroundTexture
                    , player = playerTextureSet
                    , monsterWalking = monsterWalkingSet
                    , monsterHunting = monsterHuntingSet }

hunted win directionKey randomGenerator textures glossState = mdo
    player <- transfer2 initialPlayer (\p dead dK -> movePlayer p dK dead 10) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer3 initialMonster wanderOrHunt player randomNumber gameOver'
    gameOver <- memo (playerEaten <$> player <*> monster)
    gameOver' <- delay False gameOver
    return $ renderFrame win glossState textures <$> player <*> monster <*> gameOver
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
movePlayer direction player@(Player (xpos, ypos) _) False increment
         | outsideOfLimits (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> Float -> Bool
outsideOfLimits (xmon, ymon) size = xmon > fromIntegral width/2 - size/2 ||
                                    xmon < (-(fromIntegral width)/2 + size/2) ||
                                    ymon > fromIntegral height/2 - size/2 ||
                                    ymon < (-(fromIntegral height)/2 + size/2)

move (True, _, _, _) (Player (xpos, ypos) _) increment = Player ((xpos - increment), ypos) $ Just WalkLeft
move (_, True, _, _) (Player (xpos, ypos) _) increment = Player ((xpos + increment), ypos) $ Just WalkRight
move (_, _, True, _) (Player (xpos, ypos) _) increment = Player (xpos, (ypos + increment)) $ Just WalkUp
move (_, _, _, True) (Player (xpos, ypos) _) increment = Player (xpos, (ypos - increment)) $ Just WalkDown
move (False, False, False, False) (Player (xpos, ypos) _) _ = Player (xpos, ypos) Nothing

wanderDist = 40
huntingDist = 100

wanderOrHunt _ _ True monster = monster
wanderOrHunt player (r, _) False monster = if close player monster
                                                then hunt player monster
                                                else wander r monster

close player monster = distance player monster < huntingDist^2

distance (Player (xpos, ypos) _) (Monster (xmon, ymon) _) = (xpos - xmon)^2 + (ypos - ymon)^2

-- if player is upper left quadrant, diagonal left
-- means xpos > xmon and ypos > ymon
hunt :: Player -> Monster -> Monster
hunt (Player (xpos, ypos) _) (Monster (xmon, ymon) _) = Monster ((xmon + (signum (xpos - xmon))*monsterSpeed), (ymon + (signum (ypos - ymon))*monsterSpeed)) (Hunting $ huntingDirection (signum (xpos - xmon)) (signum (ypos - ymon)))

huntingDirection (-1) (-1) = WalkLeft
huntingDirection (-1) 1 = WalkLeft
huntingDirection 1 (-1) = WalkRight
huntingDirection 1 1 = WalkRight
huntingDirection _ _ = WalkRight

-- turn in random direction
wander :: Direction -> Monster -> Monster
wander r (Monster (xmon, ymon) (Wander _ 0)) = Monster (xmon, ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) (Hunting _)) = Monster (xmon, ymon) (Wander r wanderDist)
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

renderFrame window glossState textures (Player (xpos, ypos) playerDir) (Monster (xmon, ymon) status) gameOver = do
   displayPicture (width, height) white glossState 1.0 $ 
     Pictures $ gameOngoing gameOver
                             [background textures,
                              renderPlayer xpos ypos playerDir (player textures),
                              renderMonster status xmon ymon (monsterWalking textures) (monsterHunting textures) ]
   swapBuffers window


--renderPlayer :: Float -> Float -> Maybe Direction -> TextureSet -> Picture
renderPlayer xpos ypos (Just WalkUp) textureSet = translate xpos ypos $ back textureSet
renderPlayer xpos ypos (Just WalkDown) textureSet = translate xpos ypos $ front textureSet
renderPlayer xpos ypos (Just WalkRight) textureSet = translate xpos ypos $ right textureSet
renderPlayer xpos ypos (Just WalkLeft) textureSet = translate xpos ypos $ left textureSet
renderPlayer xpos ypos Nothing textureSet = translate xpos ypos $ front textureSet

renderMonster :: MonsterStatus -> Float -> Float -> TextureSet -> TextureSet -> Picture
renderMonster (Hunting WalkLeft) xpos ypos _ textureSet = translate xpos ypos $ left textureSet
renderMonster (Hunting WalkRight) xpos ypos _ textureSet = translate xpos ypos $ right textureSet
renderMonster (Wander WalkUp _) xpos ypos textureSet _ = translate xpos ypos $ back textureSet
renderMonster (Wander WalkDown _) xpos ypos textureSet _ = translate xpos ypos $ front textureSet
renderMonster (Wander WalkLeft _) xpos ypos textureSet _ = translate xpos ypos $ left textureSet
renderMonster (Wander WalkRight _) xpos ypos textureSet _ = translate xpos ypos $ right textureSet

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
