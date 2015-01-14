{-# LANGUAGE RecursiveDo #-}
module Hunted.Game (
  hunted
) where

import Hunted.GameTypes
import Hunted.Sound
import Hunted.Graphics

import FRP.Elerea.Simple as Elerea
import Control.Applicative ((<$>), (<*>), liftA2)
import Graphics.Gloss.Data.ViewPort
import Data.Monoid ((<>))
import System.Random (random)

import Debug.Trace

initialPlayer :: Player
initialPlayer = Player (0, 0) Nothing

initialMonster :: Monster
initialMonster = Monster (200, 200) (Wander WalkUp wanderDist) 4

initialViewport :: ViewPort
initialViewport = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = viewportScale }

worldWidth :: Float
worldWidth = 2560

worldHeight :: Float
worldHeight = 1920

viewportScale = 4

playerSize = 20
monsterSize = 20
monsterSpeed = 5

wanderDist = 45
huntingDist = 200
boltRange = 20
boltSpeed = 10

hunted win (width, height) directionKey shootKey randomGenerator textures glossState sounds = mdo
    let worldDimensions = (worldWidth, worldHeight)
    player <- transfer2 initialPlayer (\p dead dK -> movePlayer p dK dead 10 worldDimensions) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer4 initialMonster (wanderOrHunt worldDimensions) player randomNumber gameOver' bolts'
    monster' <- delay initialMonster monster
    gameOver <- memo (gameEnds <$> player <*> monster)
    gameOver' <- delay Nothing gameOver
    viewport <- transfer initialViewport viewPortMove player
    statusChange <- transfer2 Nothing monitorStatusChange monster monster'
    playerScreams <- Elerea.until ((== (Just Lose)) <$> gameOver)
    monsterScreams <- Elerea.until ((== (Just Win)) <$> gameOver)
    shoot <- edgify shootKey
    bolts <- transfer3 [] (manageBolts worldDimensions) shoot player monster
    bolts' <- delay [] bolts

    let hunting = stillHunting <$> monster <*> gameOver
        renderState = RenderState <$> player <*> monster <*> gameOver <*> viewport <*> bolts
        soundState  = SoundState <$> statusChange
                                 <*> playerScreams
                                 <*> hunting
                                 <*> monsterScreams
                                 <*> (hasAny <$> shoot)
                                 <*> (boltHit <$> monster <*> bolts)

    return $ outputFunction win glossState textures (width, height) sounds <$> renderState <*> soundState
    where playerEaten player monster
              | distance player monster < (playerSize^2  :: Float) = Just Lose
              | otherwise                                          = Nothing
          monsterDies (Monster _ _ health) 
              | health == 0 = Just Win
              | otherwise   = Nothing
          gameEnds player monster = maybe (monsterDies monster) Just (playerEaten player monster)
          nextRandom (_, g) = random g

-- SignalGen (Signal Bolt)
bolt direction range position = stateful (Bolt position direction range False) moveBolt

hasAny (l, r, u, d) = l || r || u || d

moveBolt (Bolt (xpos, ypos) direction range hit) = Bolt (boltSpeed `times` (stepInDirection direction) `plus` (xpos, ypos)) direction (range - 1) hit

manageBolts fdimensions shoot player monster bolts =
    map moveBolt $ map (hitMonster monster) $ filter (boltStillGoing fdimensions) $ addNew bolts
        where addNew bolts = if hasAny shoot
                               then (Bolt (position player) (dirFrom shoot) boltRange False):bolts
                               else bolts

edgify s = do
  s' <- delay (False, False, False, False) s
  return $ s' >>= \x -> throttle x s

throttle (a, d, w, s) sig
   | a = return (False, d, w, s)
   | d = return (a, False, w, s)
   | w = return (a, d, False, s)
   | s = return (a, d, w, False)
   | otherwise = sig

-- boltStillGoing depends on the bolt range and on whether it hit the monster
--boltStillGoing :: (Float, Float) -> Monster -> Bolt -> Bool
boltStillGoing (width, height) (Bolt (x, y) _ range hit) =
    (not hit) && (range > 0) && x < width/2 && y < height/2

-- flips a switch when the bolt has hit the monster
hitMonster (Monster (xmon, ymon) _ _) (Bolt (x, y) dir range hit)
  | dist (xmon, ymon) (x, y) < (monsterSize/2)^2 = Bolt (x,y) dir range True
  | otherwise = Bolt (x,y) dir range hit

stillHunting _                         (Just _)  = False
stillHunting (Monster _ (Hunting _) 0) _     = False
stillHunting (Monster _ (Hunting _) _) Nothing = True
stillHunting _                         Nothing = False

viewPortMove :: Player -> ViewPort -> ViewPort
viewPortMove (Player (x,y) _) (ViewPort { viewPortTranslate = _, viewPortRotate = rotation, viewPortScale = scaled }) =
        ViewPort { viewPortTranslate = ((-x), (-y)), viewPortRotate = rotation, viewPortScale = scaled }

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Maybe Ending -> Float -> (Float, Float) -> Player
movePlayer _ player (Just _) _ _ = player
movePlayer direction player@(Player (xpos, ypos) _) Nothing increment dimensions
         | outsideOfLimits dimensions (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> (Float, Float) -> Float -> Bool
outsideOfLimits (width, height) (xmon, ymon) size = xmon > width/2 - size/2 ||
                                                    xmon < (-(width)/2 + size/2) ||
                                                    ymon > height/2 - size/2 ||
                                                    ymon < (-(height)/2 + size/2)

move (False, False, False, False) (Player (xpos, ypos) _) _ = Player (xpos, ypos) Nothing
move keys (Player (xpos, ypos) (Just (PlayerMovement direction n))) increment 
        | dirFrom keys == direction = Player ((xpos, ypos) `plus` increment `times` stepInDirection direction) (Just $ PlayerMovement direction ((n+1) `mod` 4))
        | otherwise                 = Player ((xpos, ypos) `plus` increment `times` stepInDirection (dirFrom keys)) (Just $ PlayerMovement (dirFrom keys) 0)
move keys (Player (xpos, ypos) Nothing) increment = Player ((xpos, ypos) `plus` increment `times` stepInDirection (dirFrom keys)) (Just $ PlayerMovement (dirFrom keys) 0)

dirFrom (l, r, u, d)
  | l = WalkLeft
  | r = WalkRight
  | u = WalkUp
  | d = WalkDown
  | otherwise = error "no direction from keys"

stepInDirection WalkLeft  = (-1, 0)
stepInDirection WalkRight = (1, 0)
stepInDirection WalkUp    = (0, 1)
stepInDirection WalkDown  = (0, -1)

hitOrMiss bolts monster@(Monster (xmon, ymon) status health) =
    Monster (xmon, ymon) status (health - (hits monster bolts))
    where hits monster bolts = fromIntegral $ length
                                            $ filter (< (monsterSize/2)^2) (boltDistances monster bolts)
boltDistances :: Monster -> [Bolt] -> [Float]
boltDistances (Monster (xmon, ymon) _ _) bolts =
    map (\(Bolt (xbolt, ybolt) _ _ _) -> dist (xmon, ymon) (xbolt, ybolt)) bolts

boltHit monster bolts = any (== True) $ map (< (monsterSize/2)^2) (boltDistances monster bolts)

--wanderOrHunt a b c d e f | trace ("monsterState " ++ show f ++ " bolts: " ++ show e) False = undefined
wanderOrHunt _ _ _ (Just _) _ monster = monster
-- no health left: dead
wanderOrHunt _ _ _ _    _ monster@(Monster _ _ 0) = monster

wanderOrHunt dimensions player (r, _) Nothing bolts monster = do
    let monsterHit = hitOrMiss bolts monster
    if close player monsterHit
     then hunt player monsterHit
     else wander r monsterHit dimensions

close player monster = distance player monster < huntingDist^2

distance :: Player -> Monster -> Float
distance (Player (xpos, ypos) _) (Monster (xmon, ymon) _ _) = dist (xpos, ypos) (xmon, ymon)

-- if player is upper left quadrant, diagonal left
-- means xpos > xmon and ypos > ymon
hunt :: Player -> Monster -> Monster
hunt (Player (xpos, ypos) _) (Monster (xmon, ymon) _ health) = Monster ((xmon + (signum (xpos - xmon))*monsterSpeed), (ymon + (signum (ypos - ymon))*monsterSpeed)) (Hunting $ huntingDirection (signum (xpos - xmon)) (signum (ypos - ymon))) health

huntingDirection (-1) (-1) = WalkLeft
huntingDirection (-1) 1 = WalkLeft
huntingDirection 1 (-1) = WalkRight
huntingDirection 1 1 = WalkRight
huntingDirection (-1) _ = WalkLeft
huntingDirection _ _ = WalkRight

-- turn in random direction
--wander :: Direction -> Monster -> Monster
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

stepInCurrentDirection direction (xpos, ypos) speed = speed `times` (stepInDirection direction) `plus` (xpos, ypos)

monitorStatusChange (Monster _ _ num) (Monster _ _ 0) _ = if num > 0 then Just Safe else Nothing
monitorStatusChange (Monster _ (Hunting _) _) (Monster _ (Wander _ _) _) _ = Just Danger
monitorStatusChange (Monster _ (Wander _ _) _) (Monster _ (Hunting _) _) _ = Just Safe
monitorStatusChange _ _ _ = Nothing

-- output functions
outputFunction window glossState textures dimensions sounds renderState soundState =  (renderFrame window glossState textures dimensions (worldWidth, worldHeight) renderState) >> (playSounds sounds soundState)
