module Hunted.Graphics (
  loadTextures
, initState
, renderFrame
) where

import Hunted.GameTypes
import Hunted.Backend (swapBuffers)

import Graphics.Gloss hiding (play)
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.ViewPort
import Control.Applicative ((<*>), (<$>))

data TextureSet = TextureSet { front :: Picture, back :: Picture, left :: Picture, right :: Picture }
                | PlayerTextureSet { fronts :: [Picture], backs :: [Picture], lefts :: [Picture], rights :: [Picture] }
data Textures = Textures { background :: Picture
                         , player :: TextureSet
                         , monsterWalking :: TextureSet
                         , monsterHunting :: TextureSet }

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


renderFrame window glossState textures dimensions (RenderState (Player (xpos, ypos) playerDir) (Monster (xmon, ymon) status) gameOver viewport) = do
   displayPicture dimensions black glossState (viewPortScale viewport) $ 
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
