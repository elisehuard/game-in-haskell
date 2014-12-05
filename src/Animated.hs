{-# LANGUAGE PackageImports, RecursiveDo #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.ViewPort
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
data Player = Player { position :: Pos, movement :: Maybe PlayerMovement }
               deriving Show
data PlayerMovement = PlayerMovement { dir :: Direction, step :: Int }
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
                | PlayerTextureSet { fronts :: [Picture], backs :: [Picture], lefts :: [Picture], rights :: [Picture] }
data Textures = Textures { background :: Picture
                         , player :: TextureSet
                         , monsterWalking :: TextureSet
                         , monsterHunting :: TextureSet }

initialPlayer :: Player
initialPlayer = Player (0, 0) Nothing

initialMonster :: Monster
initialMonster = Monster (200, 200) (Wander WalkUp wanderDist)

initialViewport :: ViewPort
initialViewport = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = viewportScale }

viewportScale = 4

width :: Int
width = 640

height :: Int
height = 480

fullWidth :: Float
fullWidth = fromIntegral width*viewportScale

fullHeight :: Float
fullHeight = fromIntegral height*viewportScale

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
    playerTextureSet <- PlayerTextureSet <$> loadAnims "images/knight-front.bmp" "images/knight-front-1.bmp" "images/knight-front-3.bmp"
                                         <*> loadAnims "images/knight-back.bmp" "images/knight-back-1.bmp" "images/knight-back-3.bmp"
                                         <*> loadAnims "images/knight-left.bmp" "images/knight-left-1.bmp" "images/knight-left-3.bmp"
                                         <*> loadAnims "images/knight-right.bmp" "images/knight-right-1.bmp" "images/knight-right-3.bmp"
    monsterWalkingSet <- TextureSet <$> loadBMP "images/monster-walking-front.bmp"
                                    <*> loadBMP "images/monster-walking-back.bmp"
                                    <*> loadBMP "images/monster-walking-left.bmp"
                                    <*> loadBMP "images/monster-walking-right.bmp"
    -- moves diagonally, so only 2 textures needed technically
    monsterHuntingSet <- TextureSet <$> loadBMP "images/monster-hunting-left.bmp"
                                    <*> loadBMP "images/monster-hunting-right.bmp"
                                    <*> loadBMP "images/monster-hunting-left.bmp"
                                    <*> loadBMP "images/monster-hunting-right.bmp"
    backgroundTexture <- loadBMP "images/background-large.bmp"
    return Textures { background = backgroundTexture
                    , player = playerTextureSet
                    , monsterWalking = monsterWalkingSet
                    , monsterHunting = monsterHuntingSet }

loadAnims :: String -> String -> String -> IO [Picture]
loadAnims path1 path2 path3 = fun <$> loadBMP path1 <*> loadBMP path2 <*> loadBMP path3
                              where fun a b c = [a,b,c]

hunted win directionKey randomGenerator textures glossState = mdo
    player <- transfer2 initialPlayer (\p dead dK -> movePlayer p dK dead 10) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer3 initialMonster wanderOrHunt player randomNumber gameOver'
    gameOver <- memo (playerEaten <$> player <*> monster)
    gameOver' <- delay False gameOver
    viewport <- transfer initialViewport viewPortMove player
    return $ renderFrame win glossState textures <$> player <*> monster <*> gameOver <*> viewport
    where playerEaten player monster = distance player monster < (10^2  :: Float)
          nextRandom (a, g) = random g

viewPortMove :: Player -> ViewPort -> ViewPort
viewPortMove (Player (x,y) _) (ViewPort { viewPortTranslate = _, viewPortRotate = rotation, viewPortScale = scaled }) =
        ViewPort { viewPortTranslate = ((-x), (-y)), viewPortRotate = rotation, viewPortScale = scaled }

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

move a b c | trace ("move " ++ show b) False = undefined
move (True, _, _, _) (Player (xpos, ypos) (Just (PlayerMovement WalkLeft n))) increment = Player (xpos - increment, ypos) (Just $ PlayerMovement WalkLeft ((n+1) `mod` 4))
move (True, _, _, _) (Player (xpos, ypos) _) increment = Player (xpos - increment, ypos) $ Just $ PlayerMovement WalkLeft 0
move (_, True, _, _) (Player (xpos, ypos) (Just (PlayerMovement WalkRight n))) increment = Player (xpos + increment, ypos) (Just $ PlayerMovement WalkRight ((n+1) `mod` 4))
move (_, True, _, _) (Player (xpos, ypos) _) increment = Player (xpos + increment, ypos) $ Just $ PlayerMovement WalkRight 0
move (_, _, True, _) (Player (xpos, ypos) (Just (PlayerMovement WalkUp n))) increment = Player (xpos, (ypos + increment)) (Just $ PlayerMovement WalkUp ((n+1) `mod` 4))
move (_, _, True, _) (Player (xpos, ypos) _) increment = Player (xpos, (ypos + increment)) $ Just $ PlayerMovement WalkUp 0
move (_, _, _, True) (Player (xpos, ypos) (Just (PlayerMovement WalkDown n))) increment = Player (xpos, (ypos - increment)) (Just $ PlayerMovement WalkDown ((n+1) `mod` 4))
move (_, _, _, True) (Player (xpos, ypos) _) increment = Player (xpos, (ypos - increment)) $ Just $ PlayerMovement WalkDown 0

move (False, False, False, False) (Player (xpos, ypos) _) _ = Player (xpos, ypos) Nothing

wanderDist = 45
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
huntingDirection (-1) _ = WalkLeft
huntingDirection _ _ = WalkRight

-- turn in random direction
--wander :: Direction -> Monster -> Monster
wander a b | trace ("wander " ++ show b) False = undefined
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

renderFrame window glossState textures (Player (xpos, ypos) playerDir) (Monster (xmon, ymon) status) gameOver viewport = do
   displayPicture (width, height) black glossState (viewPortScale viewport) $ 
     Pictures $ gameOngoing gameOver
                             [applyViewPortToPicture viewport (background textures),
                              renderPlayer playerDir (player textures),
                              uncurry translate (viewPortTranslate viewport) $ renderMonster status xmon ymon (monsterWalking textures) (monsterHunting textures) ]
   swapBuffers window


--renderPlayer :: Float -> Float -> Maybe Direction -> TextureSet -> Picture
renderPlayer (Just (PlayerMovement WalkUp 0)) textureSet = backs textureSet !! 0
renderPlayer (Just (PlayerMovement WalkUp 1)) textureSet = backs textureSet !! 1
renderPlayer (Just (PlayerMovement WalkUp 2)) textureSet = backs textureSet !! 0
renderPlayer (Just (PlayerMovement WalkUp 3)) textureSet = backs textureSet !! 2
renderPlayer (Just (PlayerMovement WalkDown 0)) textureSet = fronts textureSet !! 0
renderPlayer (Just (PlayerMovement WalkDown 1)) textureSet = fronts textureSet !! 1
renderPlayer (Just (PlayerMovement WalkDown 2)) textureSet = fronts textureSet !! 0
renderPlayer (Just (PlayerMovement WalkDown 3)) textureSet = fronts textureSet !! 2
renderPlayer (Just (PlayerMovement WalkRight 0)) textureSet = rights textureSet !! 0
renderPlayer (Just (PlayerMovement WalkRight 1)) textureSet = rights textureSet !! 1
renderPlayer (Just (PlayerMovement WalkRight 2)) textureSet = rights textureSet !! 0
renderPlayer (Just (PlayerMovement WalkRight 3)) textureSet = rights textureSet !! 2
renderPlayer (Just (PlayerMovement WalkLeft 0)) textureSet = lefts textureSet !! 0
renderPlayer (Just (PlayerMovement WalkLeft 1)) textureSet = lefts textureSet !! 1
renderPlayer (Just (PlayerMovement WalkLeft 2)) textureSet = lefts textureSet !! 0
renderPlayer (Just (PlayerMovement WalkLeft 3)) textureSet = lefts textureSet !! 2
renderPlayer Nothing textureSet = fronts textureSet !! 0

renderMonster :: MonsterStatus -> Float -> Float -> TextureSet -> TextureSet -> Picture
renderMonster (Hunting WalkLeft) xpos ypos _ textureSet = translate xpos ypos $ left textureSet
renderMonster (Hunting WalkRight) xpos ypos _ textureSet = translate xpos ypos $ right textureSet
renderMonster (Wander WalkUp n) xpos ypos textureSet _ = translate xpos ypos $ back textureSet
renderMonster (Wander WalkDown n) xpos ypos textureSet _ = translate xpos ypos $ front textureSet
renderMonster (Wander WalkLeft n) xpos ypos textureSet _ = translate xpos ypos $ rotate (16* fromIntegral n) $ left textureSet
renderMonster (Wander WalkRight n) xpos ypos textureSet _ = translate xpos ypos $ rotate (16* fromIntegral n) $ right textureSet

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
