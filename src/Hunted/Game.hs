{-# LANGUAGE RecursiveDo #-}
module Hunted.Game (
  hunted
) where

import Hunted.GameTypes
import Hunted.Sound
import Hunted.Graphics

import FRP.Elerea.Simple as Elerea
import Control.Applicative ((<$>), (<*>))
import Graphics.Gloss.Data.ViewPort
import System.Random (random)

initialPlayer :: Player
initialPlayer = Player (0, 0) Nothing

initialMonster :: Monster
initialMonster = Monster (200, 200) (Wander WalkUp wanderDist)

initialViewport :: ViewPort
initialViewport = ViewPort { viewPortTranslate = (0, 0), viewPortRotate = 0, viewPortScale = viewportScale }

viewportScale = 4

playerSize = 20
monsterSize = 20
monsterSpeed = 5

hunted win (width, height) directionKey randomGenerator textures glossState sounds = mdo
    let fdimensions = (fromIntegral width, fromIntegral height)
    player <- transfer2 initialPlayer (\p dead dK -> movePlayer p dK dead 10 fdimensions) directionKey gameOver'
    randomNumber <- stateful (undefined, randomGenerator) nextRandom
    monster <- transfer3 initialMonster (wanderOrHunt fdimensions) player randomNumber gameOver'
    monster' <- delay initialMonster monster
    gameOver <- memo (playerEaten <$> player <*> monster)
    gameOver' <- delay False gameOver
    viewport <- transfer initialViewport viewPortMove player
    statusChange <- transfer2 Nothing monitorStatusChange monster monster'
    endOfGame <- Elerea.until gameOver

    let hunting = stillHunting <$> monster <*> gameOver
        renderState = RenderState <$> player <*> monster <*> gameOver <*> viewport
        soundState  = SoundState <$> statusChange <*> endOfGame <*> hunting

    return $ outputFunction win glossState textures (width, height) sounds <$> renderState <*> soundState
    where playerEaten player monster = distance player monster < (playerSize^2  :: Float)
          nextRandom (a, g) = random g

stillHunting _                       True  = False
stillHunting (Monster _ (Hunting _)) False = True
stillHunting _                       False = False

viewPortMove :: Player -> ViewPort -> ViewPort
viewPortMove (Player (x,y) _) (ViewPort { viewPortTranslate = _, viewPortRotate = rotation, viewPortScale = scaled }) =
        ViewPort { viewPortTranslate = ((-x), (-y)), viewPortRotate = rotation, viewPortScale = scaled }

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Bool -> Float -> (Float, Float) -> Player
movePlayer _ player True _ _ = player
movePlayer direction player@(Player (xpos, ypos) _) False increment dimensions
         | outsideOfLimits dimensions (position (move direction player increment)) playerSize = player
         | otherwise = move direction player increment

outsideOfLimits :: (Float, Float) -> (Float, Float) -> Float -> Bool
outsideOfLimits (width, height) (xmon, ymon) size = xmon > width/2 - size/2 ||
                                                    xmon < (-(width)/2 + size/2) ||
                                                    ymon > height/2 - size/2 ||
                                                    ymon < (-(height)/2 + size/2)

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
huntingDist = 200

wanderOrHunt _ _ _ True monster = monster
wanderOrHunt dimensions player (r, _) False monster = if close player monster
                                                       then hunt player monster
                                                       else wander r monster dimensions

close player monster = distance player monster < huntingDist^2

distance :: Player -> Monster -> Float
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
wander r (Monster (xmon, ymon) (Wander _ 0)) _ = Monster (xmon, ymon) (Wander r wanderDist)
wander r (Monster (xmon, ymon) (Hunting _))  _ = Monster (xmon, ymon) (Wander r wanderDist)
-- go straight
wander _ (Monster (xmon, ymon) (Wander direction n)) dimensions = do
                   let currentDirection = continueDirection direction (outsideOfLimits dimensions (xmon, ymon) monsterSize)
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


monitorStatusChange (Monster _ (Hunting _)) (Monster _ (Wander _ _)) pace = Just Danger
monitorStatusChange (Monster _ (Wander _ _)) (Monster _ (Hunting _)) pace = Just Safe
monitorStatusChange _ _ pace = Nothing

-- output functions
outputFunction window glossState textures dimensions sounds renderState soundState =  (renderFrame window glossState textures dimensions renderState) >> (playSounds sounds soundState)
