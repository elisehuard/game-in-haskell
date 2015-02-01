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
                | PlayerTextureSet { fronts :: WalkingTexture, backs :: WalkingTexture, lefts :: WalkingTexture, rights :: WalkingTexture }

data WalkingTexture = WalkingTexture { neutral :: Picture, walkLeft :: Picture, walkRight :: Picture }

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
    backgroundTexture <- loadBMP "images/background-tile.bmp"
    return Textures { background = backgroundTexture
                    , player = playerTextureSet
                    , monsterWalking = monsterWalkingSet
                    , monsterHunting = monsterHuntingSet }

loadAnims :: String -> String -> String -> IO WalkingTexture
loadAnims path1 path2 path3 = WalkingTexture <$> loadBMP path1 <*> loadBMP path2 <*> loadBMP path3

{- again, need to export gloss internal state for this signature, pull request required
renderFrame :: Window
               -> gloss-rendering-1.9.2.1:Graphics.Gloss.Internals.Rendering.State.State
               -> Textures
               -> (Int, Int)
               -> (Float, Float)
               -> RenderState
               -> IO ()
-}
renderFrame window glossState textures dimensions (worldWidth, worldHeight) (RenderState (Player _ playerDir) monster gameOver viewport bolts lives score) = do
   displayPicture dimensions black glossState (viewPortScale viewport) $
     Pictures $ gameOngoing gameOver lives $ gameStats lives score $
                             [ uncurry translate (viewPortTranslate viewport) $ tiledBackground (background textures) worldWidth worldHeight
                             , renderPlayer playerDir (player textures)
                             , uncurry translate (viewPortTranslate viewport) $ renderMonster monster (monsterWalking textures) (monsterHunting textures)
                             , uncurry translate (viewPortTranslate viewport) $ renderHealthBar monster ]
                              ++ (map (uncurry translate (viewPortTranslate viewport) . renderBolt) bolts)
   swapBuffers window

renderFrame window glossState _ dimensions _ StartRenderState = do
  displayPicture dimensions black glossState 1 $
    Pictures [ Color green $ translate (-140) 0 $ scale 0.4 0.4 $ Text "Hunting Season"
             , Color green $ translate (-140) (-50) $ scale 0.1 0.1 $ Text "Press s to get started" ]
  swapBuffers window

-- tiling: pictures translated to the appropriate locations to fill up the given width and heights
-- I scaled the tile to the greatest common factor of the width and height, but it should work to fit the actual width and height
-- which potentially means translating the tiles back a bit not to go over the edge
tileSize :: Float
tileSize = 160

tiledBackground :: Picture -> Float -> Float -> Picture
tiledBackground texture width height = Pictures $ map (\a ->  ((uncurry translate) a) texture) $ translateMatrix width height

-- what we want: 640, 480
-- -320--x--(-160)--x--0--x--160--x--320
--      -240      -80    80      240
-- -240--x--(-80)--x--80--x--240
--      -160       0     160
translateMatrix :: Float -> Float -> [(Float, Float)]
translateMatrix w h = concat $ map (zip xTiles)
                             $ map (replicate (length xTiles)) yTiles
                      where xTiles = [lowerbound w, lowerbound w + tileSize..higherbound w]
                            yTiles = [lowerbound h, lowerbound h + tileSize..higherbound h]
                            higherbound size = size/2 - tileSize/2
                            lowerbound size = -(higherbound size)

renderPlayer :: Maybe PlayerMovement -> TextureSet -> Picture
renderPlayer (Just (PlayerMovement WalkUp One)) textureSet = neutral $ backs textureSet
renderPlayer (Just (PlayerMovement WalkUp Two)) textureSet = walkLeft $ backs textureSet
renderPlayer (Just (PlayerMovement WalkUp Three)) textureSet = neutral $ backs textureSet
renderPlayer (Just (PlayerMovement WalkUp Four)) textureSet = walkRight $ backs textureSet
renderPlayer (Just (PlayerMovement WalkDown One)) textureSet = neutral $ fronts textureSet
renderPlayer (Just (PlayerMovement WalkDown Two)) textureSet = walkLeft $ fronts textureSet
renderPlayer (Just (PlayerMovement WalkDown Three)) textureSet = neutral $ fronts textureSet
renderPlayer (Just (PlayerMovement WalkDown Four)) textureSet = walkRight $ fronts textureSet
renderPlayer (Just (PlayerMovement WalkRight One)) textureSet = neutral $ rights textureSet
renderPlayer (Just (PlayerMovement WalkRight Two)) textureSet = walkLeft $ rights textureSet
renderPlayer (Just (PlayerMovement WalkRight Three)) textureSet = neutral $ rights textureSet
renderPlayer (Just (PlayerMovement WalkRight Four)) textureSet = walkRight $ rights textureSet
renderPlayer (Just (PlayerMovement WalkLeft One)) textureSet = neutral $ lefts textureSet
renderPlayer (Just (PlayerMovement WalkLeft Two)) textureSet = walkLeft $ lefts textureSet
renderPlayer (Just (PlayerMovement WalkLeft Three)) textureSet = neutral $ lefts textureSet
renderPlayer (Just (PlayerMovement WalkLeft Four)) textureSet = walkRight $ lefts textureSet
renderPlayer Nothing textureSet = neutral $ fronts textureSet

renderMonster :: Monster -> TextureSet -> TextureSet -> Picture
renderMonster (Monster (xpos, ypos) (Hunting WalkLeft) _) _ textureSet = translate xpos ypos $ left textureSet
renderMonster (Monster (xpos, ypos) (Hunting WalkRight) _) _ textureSet = translate xpos ypos $ right textureSet
renderMonster (Monster (xpos, ypos) (Hunting WalkUp) _) _ textureSet = translate xpos ypos $ back textureSet
renderMonster (Monster (xpos, ypos) (Hunting WalkDown) _) _ textureSet = translate xpos ypos $ front textureSet
renderMonster (Monster (xpos, ypos) (Wander WalkUp _) _) textureSet _ = translate xpos ypos $ back textureSet
renderMonster (Monster (xpos, ypos) (Wander WalkDown _) _) textureSet _ = translate xpos ypos $ front textureSet
renderMonster (Monster (xpos, ypos) (Wander WalkLeft n) _) textureSet _ = translate xpos ypos $ rotate (16* fromIntegral n) $ left textureSet
renderMonster (Monster (xpos, ypos) (Wander WalkRight n) _) textureSet _ = translate xpos ypos $ rotate ((-16)* fromIntegral n) $ right textureSet

renderBolt :: Bolt -> Picture
renderBolt (Bolt (xpos, ypos) _ _ _) = translate xpos ypos $ Color (greyN 0.7) $ circleSolid 5

-- [x x x x x]
-- [0 0]
-- 1 centered around xmon, size bar
-- 2 centered around xmon - bar/2 + health/2
numberOfLives :: Float
numberOfLives = 4

healthBarLength :: Float
healthBarLength = 40

healthBarWidth :: Float
healthBarWidth = 5

renderHealthBar :: Monster -> Picture
renderHealthBar (Monster (xmon, ymon) _ health) = Pictures [ translate xmon (ymon + 30) $ Color black $ rectangleSolid healthBarLength healthBarWidth
                                                           , translate (xmon - healthBarLength/2 + health*healthBarLength/(numberOfLives*2)) (ymon + 30) $ Color red $ rectangleSolid (health*healthBarLength/numberOfLives) healthBarWidth ]

-- adds gameover text if appropriate
gameOngoing :: Maybe Ending -> Int -> [Picture] -> [Picture]
gameOngoing (Just Lose) 1 pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
gameOngoing (Just Lose) _ pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Aaargh"]
gameOngoing (Just Win) _ pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "You win!"]
gameOngoing Nothing _ pics =  pics

-- add score and lives
-- lives are reprented by circles
gameStats :: Int -> Float -> [Picture] -> [Picture]
gameStats lives score pics = pics ++ [ Color black $ translate 280 200 $ Scale 0.2 0.2 $ Text $ show score
                                     , Color black $ translate (-300) 200 $ Scale 0.2 0.2 $ Text "lives: "]
                                  ++ map (\i -> Color red $ translate ((-230) + 40*i) 210 $ circleSolid 10) [0..(fromIntegral (lives - 1))]
