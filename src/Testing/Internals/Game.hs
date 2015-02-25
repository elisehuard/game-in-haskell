{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Testing.Internals.Game (
  hunted
, monsterHits
, worldWidth
, worldHeight
) where

import Testing.GameTypes
import Testing.Sound
import Testing.Graphics

import FRP.Elerea.Simple as Elerea
import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Data.Maybe (mapMaybe)
import Data.Foldable (foldl')
import Graphics.Gloss.Data.ViewPort
import System.Random (random, RandomGen(..), randomRs)

initialPlayer :: Player
initialPlayer = Player (0, 0) Nothing Nothing

initialMonster :: (Int, Int) -> Monster
initialMonster pos = Monster pos (Wander WalkUp wanderDist) 4

initialViewport :: ViewPort
initialViewport = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = viewportScale }

worldWidth :: Int
worldWidth = 2560

worldHeight :: Int
worldHeight = 1920

viewportScale :: Float
viewportScale = 4

playerSpeed :: Int
playerSpeed = 10

playerSize :: Int
playerSize = 20

monsterSize :: Int
monsterSize = 20

monsterSpeed :: Int
monsterSpeed = 5

wanderDist :: Int
wanderDist = 45

huntingDist :: Int
huntingDist = 200

boltRange :: Range
boltRange = 20

boltSpeed :: Int
boltSpeed = 20

initialLevel :: LevelStatus
initialLevel = Level 1

initialLives :: Int
initialLives = 3

{-
-- GlossState needs to be exported
--   Graphics.Gloss.Internals.Rendering.State
--   pull request required
-}
-- expected:
hunted win windowSize directionKey shootKey randomGenerator textures glossState sounds = mdo
  let mkGame = playGame windowSize directionKey shootKey randomGenerator
  (gameState, gameTrigger) <- switcher $ mkGame <$> gameStatus'
  gameStatus <- transfer Start gameProgress gameTrigger
  gameStatus' <- delay Start gameStatus
  return $ outputFunction win glossState textures sounds <$> gameState
  where gameProgress False s      = s
        gameProgress True  Start  = InGame
        gameProgress True  InGame = Start


playGame :: RandomGen t =>
            Signal (Int, Int)
         -> Signal (Bool, Bool, Bool, Bool)
         -> Signal (Bool, Bool, Bool, Bool)
         -> t
         -> GameStatus
         -> SignalGen (Signal GameState, Signal Bool)
-- start game when pressing s
playGame windowSize _ shootKey _ Start = mdo
    let startGame = sIsPressed <$> shootKey
        renderState = StartRenderState <$> windowSize
    return (GameState <$> renderState <*> pure StartSoundState, startGame)
    where sIsPressed (_,_,_,s) = s

-- bool should be gameOver
playGame windowSize directionKey shootKey randomGenerator InGame = mdo
  (gameState, levelTrigger) <- switcher $ playLevel windowSize directionKey shootKey randomGenerator <$> levelCount' <*> score' <*> lives'
  levelCount <- transfer2 initialLevel levelProgression gameState levelTrigger
  levelCount' <- delay initialLevel levelCount
  lives <- transfer2 initialLives decrementLives gameState levelTrigger
  lives' <- delay initialLives lives
  score <- memo (stateScore <$> gameState)
  score' <- delay 0 score
  let gameOver = isGameOver <$> gameState
  return (gameState, gameOver)
  where isGameOver (GameState (RenderState {renderState_lives = l}) _) = l == 0
        isGameOver (GameState (StartRenderState _) _) = False
        stateScore (GameState (RenderState {renderState_score = s}) _) = s
        stateScore (GameState (StartRenderState _) _) = 0
        decrementLives (GameState (RenderState {renderState_ending = Just Lose}) _) True l = l - 1
        decrementLives (GameState _ _) _ l = l

-- level progression if triggered AND the player won
levelProgression :: GameState -> Bool -> LevelStatus -> LevelStatus
levelProgression _                                                            False level     = level
levelProgression (GameState (RenderState {renderState_ending = Just Win}) _)  True  (Level n) = Level (n + 1)
levelProgression (GameState (RenderState {renderState_ending = Just Lose}) _) True  level     = level
levelProgression (GameState (RenderState {renderState_ending = Nothing}) _)   True  level     = level
levelProgression (GameState (StartRenderState _) _)                           _     level     = level

switcher :: Signal (SignalGen (Signal GameState, Signal Bool)) -> SignalGen (Signal GameState, Signal Bool)
switcher levelGen = mdo
  trigger <- memo (snd =<< gameSignal)
  trigger' <- delay True trigger
  maybeSignal <- generator (toMaybe <$> trigger' <*> levelGen)
  gameSignal <- transfer undefined store maybeSignal
  return (fst =<< gameSignal, trigger)
  where store (Just x) _ = x
        store Nothing x = x
        toMaybe bool x = if bool then Just <$> x else pure Nothing

{-
playLevel :: RandomGen t =>
             Signal (Int, Int)
          -> Signal (Bool, Bool, Bool, Bool)
          -> Signal (Bool, Bool, Bool, Bool)
          -> t
          -> LevelStatus
          -> Float
          -> Int
          -> SignalGen (Signal GameState, Signal Bool)
-}
playLevel windowSize directionKey shootKey randomGenerator level@(Level n) currentScore lives = mdo
    -- render signals
    let worldDimensions = (worldWidth, worldHeight)
        randomWidths = take n $ randomRs ((-worldWidth) `quot` 2 + monsterSize `quot` 2, (worldWidth `quot` 2) - monsterSize `quot` 2) randomGenerator :: [Int]
        randomHeights = take n $ randomRs ((-worldWidth) `quot` 2 + monsterSize `quot` 2, (worldWidth `quot` 2) - monsterSize `quot` 2) randomGenerator :: [Int]
        monsterPositions = zip randomWidths randomHeights
    player <- transfer3 initialPlayer (movePlayer playerSpeed worldDimensions) directionKey levelOver' shootKey
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    hits <- memo (fmap <$> (monsterHits <$> bolts') <*> monsters')
    monsters <- transfer4 (fmap initialMonster monsterPositions) (monsterWanderings worldDimensions) player randomNumber levelOver' hits
    monsters' <- delay (map initialMonster monsterPositions) monsters
    score <- transfer currentScore accumulateScore hits
    levelOver <- memo (levelEnds <$> player <*> monsters)
    levelOver' <- delay Nothing levelOver
    animation <- transfer Nothing (endAnimation level) levelOver
    viewport <- transfer initialViewport viewPortMove player

    shoot <- edgify shootKey
    let bolt direction range startPosition = stateful (Bolt startPosition direction range False) moveBolt
        mkShot shot currentPlayer = if hasAny shot
                              then (:[]) <$> bolt (dirFrom shot) boltRange (position currentPlayer)
                              else return []
    newBolts <- generator (mkShot <$> shoot <*> player)
    bolts <- collection newBolts (boltIsAlive worldDimensions <$> monsters)
    bolts' <- delay [] bolts

    -- sound signals
    statusChange <- transfer3 Nothing safeOrDanger monsters monsters' levelOver
    playerScreams <- Elerea.until ((== (Just Lose)) <$> levelOver)
    monsterScreams <- Elerea.until ((== (Just Win)) <$> levelOver)


    let monsterIsHunting = (foldr (||) False) <$> (fmap <$> (stillHunting <$> levelOver) <*> monsters)
        renderState = RenderState <$> player
                                  <*> monsters
                                  <*> levelOver
                                  <*> viewport
                                  <*> bolts
                                  <*> pure lives
                                  <*> score
                                  <*> animation
                                  <*> windowSize
        soundState  = SoundState <$> statusChange
                                 <*> playerScreams
                                 <*> monsterIsHunting
                                 <*> monsterScreams
                                 <*> (hasAny <$> shoot)
                                 <*> (boltHit <$> monsters <*> bolts)

    return (GameState <$> renderState <*> soundState, animationEnd <$> animation)
    where playerEaten player monsters
              | any (\monster -> distance player monster < playerSize^2) monsters = Just Lose
              | otherwise                                                                     = Nothing
          monstersDead monsters
              | all monsterDead monsters = Just Win
              | otherwise                = Nothing
          monsterDead (Monster _ _ health) = health == 0
          levelEnds player monsters = maybe (monstersDead monsters) Just (playerEaten player monsters)
          nextRandom (_, g) = random g

-- FRP

collection :: (Signal [Signal Bolt]) -> Signal (Bolt -> Bool) -> SignalGen (Signal [Bolt])
collection source isAlive = mdo
  boltSignals <- delay [] (map snd <$> boltsAndSignals')
  -- bolts: SignalGen [Signal Bolt])
  -- add new bolt signals
  bolts <- memo (liftA2 (++) source boltSignals)
  -- boltsAndSignals type: SignalGen (Signal [Bolt], [Signal Bolt])
  let boltsAndSignals = zip <$> (sequence =<< bolts) <*> bolts
  -- filter out
  boltsAndSignals' <- memo (filter <$> ((.fst) <$> isAlive) <*> boltsAndSignals)
  -- return
  return $ map fst <$> boltsAndSignals'

-- FRP

hasAny :: (Bool, Bool, Bool, Bool) -> Bool
hasAny (l, r, u, d) = l || r || u || d

endAnimation :: LevelStatus -> Maybe Ending -> Maybe Animation -> Maybe Animation
endAnimation _         _           (Just (DeathAnimation 0))     = Just (DeathAnimation 0)
endAnimation _         _           (Just (NextLevelAnimation l 0)) = Just (NextLevelAnimation l 0)
endAnimation _         _           (Just (DeathAnimation n))     = Just (DeathAnimation (n - 1))
endAnimation _         _           (Just (NextLevelAnimation l n)) = Just (NextLevelAnimation l (n - 1))
endAnimation _         (Just Lose) _                           = Just $ DeathAnimation 50
endAnimation (Level n) (Just Win)  _                           = Just $ NextLevelAnimation (Level (n+1)) 50
endAnimation _         _           Nothing                     = Nothing

animationEnd :: Maybe Animation -> Bool
animationEnd (Just (DeathAnimation 0)) = True
animationEnd (Just (NextLevelAnimation _ 0)) = True
animationEnd _ = False

moveBolt :: Bolt -> Bolt
moveBolt (Bolt (xpos, ypos) direction range alreadyHit) = Bolt (boltSpeed `times` (stepInDirection direction) `plus` (xpos, ypos)) direction (range - 1) alreadyHit

boltIsAlive :: (Int, Int) -> [Monster] -> Bolt -> Bool
boltIsAlive worldDimensions monsters bolt = (not (any (\monster -> hasHit monster bolt) monsters)) && boltStillGoing worldDimensions bolt

-- let it come closer so that the hit can be registered before removing the bolt
hasHit :: Monster -> Bolt -> Bool
hasHit (Monster (xmon, ymon) _ _) (Bolt (x, y) _ _ _)
  | dist (xmon, ymon) (x, y) < ((monsterSize `quot` 4)^2) = True
  | otherwise = False

edgify :: Signal (Bool, Bool, Bool, Bool)
       -> SignalGen (Signal (Bool, Bool, Bool, Bool))
edgify s = do
  s' <- delay (False, False, False, False) s
  return $ s' >>= \x -> throttle x s

throttle :: (Bool, Bool, Bool, Bool) -> Signal (Bool, Bool, Bool, Bool) -> Signal (Bool, Bool, Bool, Bool)
throttle shot sig
   | hasAny shot = return (False, False, False, False)
   | otherwise = sig

-- boltStillGoing depends on the bolt range and on whether it hit the monster
boltStillGoing :: (Int, Int) -> Bolt -> Bool
boltStillGoing (width, height) (Bolt (x, y) _ range alreadyHit) =
    (not alreadyHit) && (range > 0) && x < width `quot` 2 && y < height `quot` 2

stillHunting :: Maybe Ending -> Monster -> Bool
stillHunting (Just _) _                         = False
stillHunting _        (Monster _ (Hunting _) 0) = False
stillHunting Nothing  (Monster _ (Hunting _) _) = True
stillHunting Nothing  _                         = False

viewPortMove :: Player -> ViewPort -> ViewPort
viewPortMove (Player (x,y) _ _) (ViewPort { viewPortTranslate = _, viewPortRotate = rotation, viewPortScale = scaled }) =
        ViewPort { viewPortTranslate = (fromIntegral (-x), fromIntegral (-y)), viewPortRotate = rotation, viewPortScale = scaled }

movePlayer :: Int
           -> (Int, Int)
           -> (Bool, Bool, Bool, Bool)
           -> Maybe Ending
           -> (Bool, Bool, Bool, Bool)
           -> Player
           -> Player
movePlayer _ _ _ (Just _) _ player = player
movePlayer increment dimensions direction Nothing shootDir player
         | outsideOfLimits dimensions (position (move direction shootDir player increment)) playerSize = player
         | otherwise = move direction shootDir player increment

outsideOfLimits :: (Int, Int) -> (Int, Int) -> Int -> Bool
outsideOfLimits (width, height) (xmon, ymon) size = xmon > width `quot` 2 - size `quot` 2 ||
                                                    xmon < (-(width) `quot` 2 + size `quot` 2) ||
                                                    ymon > height `quot` 2 - size `quot` 2 ||
                                                    ymon < (-(height) `quot` 2 + size `quot` 2)

move :: (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool) -> Player -> Int -> Player
move (False, False, False, False) sK (Player (xpos, ypos) _ _) _ = Player (xpos, ypos) Nothing (crossbowPointed sK)
move keys sK (Player (xpos, ypos) (Just (PlayerMovement direction n)) _) increment
        | dirFrom keys == direction = Player ((xpos, ypos) `plus` increment `times` stepInDirection direction) (Just $ PlayerMovement direction (circular n)) (crossbowPointed sK)
        | otherwise                 = Player ((xpos, ypos) `plus` increment `times` stepInDirection (dirFrom keys)) (Just $ PlayerMovement (dirFrom keys) One) (crossbowPointed sK)
move keys sK (Player (xpos, ypos) Nothing _) increment = Player ((xpos, ypos) `plus` increment `times` stepInDirection (dirFrom keys)) (Just $ PlayerMovement (dirFrom keys) One) (crossbowPointed sK)

crossbowPointed :: (Bool, Bool, Bool, Bool) -> Maybe Direction
crossbowPointed (a,d,w,s)
    | w = Just WalkUp
    | s = Just WalkDown
    | a = Just WalkLeft
    | d = Just WalkRight
    | otherwise = Nothing

dirFrom :: (Bool, Bool, Bool, Bool) -> Direction
dirFrom (l, r, u, d)
  | l = WalkLeft
  | r = WalkRight
  | u = WalkUp
  | d = WalkDown
  | otherwise = error "no direction from keys"

stepInDirection :: Direction -> (Int, Int)
stepInDirection WalkLeft  = (-1, 0)
stepInDirection WalkRight = (1, 0)
stepInDirection WalkUp    = (0, 1)
stepInDirection WalkDown  = (0, -1)

hitOrMiss :: Int -> Monster -> Monster
hitOrMiss hits (Monster (xmon, ymon) status health) =
    Monster (xmon, ymon) status (health - hits)

monsterHits :: [Bolt] -> Monster -> Int
monsterHits bolts monster = fromIntegral $ length
                                         $ filter (<= (monsterSize `quot` 2)^2) (boltDistances monster (filter notCounted bolts))
                                         where notCounted (Bolt _ _ _ alreadyHit) = not alreadyHit

accumulateScore :: [Int] -> Int -> Int
accumulateScore hits score = score + sum hits

boltDistances :: Monster -> [Bolt] -> [Int]
boltDistances (Monster (xmon, ymon) _ _) bolts =
    map (\(Bolt (xbolt, ybolt) _ _ _) -> dist (xmon, ymon) (xbolt, ybolt)) bolts

boltHit :: [Monster] -> [Bolt] -> Bool
boltHit monsters bolts = any (== True) $ concat $ map (\monster -> map (< (monsterSize `quot` 2)^2) (boltDistances monster bolts)) monsters

monsterWanderings :: RandomGen t => (Int, Int) -> Player -> (Direction, t) -> Maybe Ending -> [Int] -> [Monster] -> [Monster]
monsterWanderings dim p gen ending hits monsters = map (wanderOrHunt dim p gen ending) (zip hits monsters)

wanderOrHunt :: System.Random.RandomGen t =>
                (Int, Int)
                -> Player
                -> (Direction, t)
                -> Maybe Ending
                -> (Int, Monster)
                -> Monster
-- game ended
wanderOrHunt _ _ _ (Just _) (_, monster) = monster

-- no health left: dead
wanderOrHunt _ _ _ _        (_, monster@(Monster _ _ 0)) = monster

-- normal game
wanderOrHunt dimensions player (r, _) Nothing (hits, monster) = do
    let monsterHit = hitOrMiss hits monster
    if close player monsterHit
     then hunt player monsterHit
     else wander r monsterHit dimensions

close :: Player -> Monster -> Bool
close player monster = distance player monster < huntingDist^2

distance :: Player -> Monster -> Int
distance (Player (xpos, ypos) _ _) (Monster (xmon, ymon) _ _) = dist (xpos, ypos) (xmon, ymon)

-- if player is upper left quadrant, diagonal left
-- means xpos > xmon and ypos > ymon
hunt :: Player -> Monster -> Monster
hunt (Player (xpos, ypos) _ _) (Monster (xmon, ymon) _ health) = Monster ((xmon + (signum (xpos - xmon))*monsterSpeed), (ymon + (signum (ypos - ymon))*monsterSpeed)) (Hunting $ huntingDirection (signum (xpos - xmon)) (signum (ypos - ymon))) health

huntingDirection :: Int -> Int -> Direction
huntingDirection (-1) (-1) = WalkLeft
huntingDirection (-1) 1 = WalkLeft
huntingDirection 1 (-1) = WalkRight
huntingDirection 1 1 = WalkRight
huntingDirection (-1) _ = WalkLeft
huntingDirection _ _ = WalkRight

-- turn in random direction
wander :: Direction -> Monster -> (Int, Int) -> Monster
wander r (Monster (xmon, ymon) (Wander _ 0) health) _ = Monster (xmon, ymon) (Wander r wanderDist) health
wander r (Monster (xmon, ymon) (Hunting _) health)  _ = Monster (xmon, ymon) (Wander r wanderDist) health
-- go straight
wander _ (Monster (xmon, ymon) (Wander direction n) health) dimensions = do
                   let currentDirection = continueDirection direction (outsideOfLimits dimensions (xmon, ymon) monsterSize)
                   Monster
                       (stepInCurrentDirection currentDirection (xmon, ymon) monsterSpeed)
                       (Wander currentDirection (n-1))
                       health

continueDirection :: Direction -> Bool -> Direction
continueDirection WalkUp True = WalkDown
continueDirection WalkDown True = WalkUp
continueDirection WalkLeft True = WalkRight
continueDirection WalkRight True = WalkLeft
continueDirection direction False = direction

stepInCurrentDirection :: Direction -> (Int, Int) -> Int -> Pos
stepInCurrentDirection direction (xpos, ypos) speed = speed `times` (stepInDirection direction) `plus` (xpos, ypos)

safeOrDanger :: [Monster] -> [Monster] -> Maybe Ending -> Maybe StatusChange -> Maybe StatusChange
safeOrDanger _ _ (Just _) _ = Just Safe
safeOrDanger monsters monsters' _ _ = do
  let statusChanges = mapMaybe monitorStatusChange (zip monsters monsters')
  foldl' dominatingChanges Nothing statusChanges
  where dominatingChanges _             Danger = Just Danger
        dominatingChanges (Just Danger) Safe = Just Danger
        dominatingChanges _             Safe = Just Safe

monitorStatusChange :: (Monster, Monster) -> Maybe StatusChange
monitorStatusChange ((Monster _ _ num), (Monster _ _ 0)) = if num > 0 then Just Safe else Nothing
monitorStatusChange ((Monster _ (Hunting _) _), (Monster _ (Wander _ _) _)) = Just Danger
monitorStatusChange ((Monster _ (Wander _ _) _), (Monster _ (Hunting _) _)) = Just Safe
monitorStatusChange _ = Nothing

-- output functions
outputFunction window glossState textures sounds (GameState renderState soundState) =
  (renderFrame window glossState textures (worldWidth, worldHeight) renderState) >> playSounds sounds soundState
