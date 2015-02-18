{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
import qualified Data.Map.Strict as Map

data TextureSet = TextureSet { front :: Picture, back :: Picture, left :: Picture, right :: Picture }
                | PlayerTextureSet { fronts :: WalkingTexture, backs :: WalkingTexture, lefts :: WalkingTexture, rights :: WalkingTexture }

data WalkingTexture = WalkingTexture { neutral :: Picture, walkLeft :: Picture, walkRight :: Picture }

data Textures = Textures { background :: Picture
                         , player :: TextureSet
                         , monsterWalking :: TextureSet
                         , monsterHunting :: TextureSet
                         , texts :: Map.Map String Picture
                         , boltTextures :: TextureSet }

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
    boltSet <- TextureSet <$> loadBMP "images/bolt-down.bmp"
                          <*> loadBMP "images/bolt-up.bmp"
                          <*> loadBMP "images/bolt-left.bmp"
                          <*> loadBMP "images/bolt-right.bmp"
    backgroundTexture <- loadBMP "images/background-tile.bmp"
    gameOverText <- loadBMP "images/game-over.bmp"
    return Textures { background = backgroundTexture
                    , player = playerTextureSet
                    , monsterWalking = monsterWalkingSet
                    , monsterHunting = monsterHuntingSet
                    , texts = Map.singleton "game-over" gameOverText
                    , boltTextures = boltSet }

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
renderFrame window
            glossState
            textures
            (worldWidth, worldHeight)
            (RenderState (Player _ playerDir)
                         monsters
                         gameOver
                         viewport
                         bolts
                         lives
                         score
                         mbAnimation
                         dimensions) = do
   displayPicture dimensions black glossState (viewPortScale viewport) $
     Pictures $ animation mbAnimation dimensions $ gameOngoing gameOver lives (texts textures) $ gameStats lives score dimensions $
                             [ uncurry translate (viewPortTranslate viewport) $ tiledBackground (background textures) worldWidth worldHeight
                             , renderPlayer playerDir (player textures)
                             , Pictures $ map (uncurry translate (viewPortTranslate viewport) . (renderBolt (boltTextures textures))) bolts
                             , uncurry translate (viewPortTranslate viewport) $ Pictures $ map (renderMonster (monsterWalking textures) (monsterHunting textures)) monsters
                             , uncurry translate (viewPortTranslate viewport) $ Pictures $ map renderHealthBar monsters ]
   swapBuffers window

renderFrame window glossState _ _ (StartRenderState dimensions) = do
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
renderPlayer (Just (PlayerMovement dir One)) textureSet = neutral $ playerDirectionTexture dir textureSet
renderPlayer (Just (PlayerMovement dir Two)) textureSet = walkLeft $ playerDirectionTexture dir textureSet
renderPlayer (Just (PlayerMovement dir Three)) textureSet = neutral $ playerDirectionTexture dir textureSet
renderPlayer (Just (PlayerMovement dir Four)) textureSet = walkRight $ playerDirectionTexture dir textureSet
renderPlayer Nothing textureSet = neutral $ fronts textureSet

renderMonster :: TextureSet -> TextureSet -> Monster -> Picture
renderMonster _ textureSet (Monster (xpos, ypos) (Hunting dir) _) = translate xpos ypos $ directionTexture dir textureSet
renderMonster textureSet _ (Monster (xpos, ypos) (Wander WalkUp _) _) = translate xpos ypos $ back textureSet
renderMonster textureSet _ (Monster (xpos, ypos) (Wander WalkDown _) _) = translate xpos ypos $ front textureSet
renderMonster textureSet _ (Monster (xpos, ypos) (Wander WalkLeft n) _) = translate xpos ypos $ rotate (16* fromIntegral n) $ left textureSet
renderMonster textureSet _ (Monster (xpos, ypos) (Wander WalkRight n) _) = translate xpos ypos $ rotate ((-16)* fromIntegral n) $ right textureSet

renderBolt :: TextureSet -> Bolt -> Picture
renderBolt textureSet (Bolt (xpos, ypos) dir _ _) = translate xpos ypos $ directionTexture dir textureSet

directionTexture :: Direction -> TextureSet -> Picture
directionTexture WalkUp = back
directionTexture WalkDown = front
directionTexture WalkLeft = left
directionTexture WalkRight = right

playerDirectionTexture :: Direction -> TextureSet -> WalkingTexture
playerDirectionTexture WalkUp = fronts
playerDirectionTexture WalkDown = backs
playerDirectionTexture WalkLeft = lefts
playerDirectionTexture WalkRight = rights
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
gameOngoing :: Maybe Ending -> Int -> Map.Map String Picture -> [Picture] -> [Picture]
gameOngoing (Just Lose) 1 texts pics =  pics ++ [translate (-50) 0 $ (texts Map.! "game-over")]
gameOngoing (Just Lose) _ _     pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Aaargh"]
gameOngoing (Just Win)  _ _     pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "You win!"]
gameOngoing Nothing     _ _     pics =  pics

-- add score and lives
-- lives are reprented by circles
gameStats :: Int -> Float -> (Int, Int) -> [Picture] -> [Picture]
gameStats lives score (w, h) pics = do
  let fWidth = fromIntegral w
      fHeight = fromIntegral h
  pics ++ [ Color black $ translate (fWidth/2 - 80) (fHeight/2 - 50) $ Scale 0.2 0.2 $ Text $ show $ round score
          , Color black $ translate ((-fWidth)/2 + 20) (fHeight/2 - 50) $ Scale 0.2 0.2 $ Text "lives: "]
       ++ map (\i -> Color red $ translate ((-fWidth)/2 + 90 + 40*i) (fHeight/2 - 40) $ circleSolid 10) [0..(fromIntegral (lives - 1))]

animation :: Maybe Animation -> (Int, Int) -> [Picture] -> [Picture]
animation Nothing                         _      pics = pics
animation (Just (DeathAnimation _))       _      pics = pics
animation (Just (NextLevelAnimation l n)) (w, h) pics = pics ++
                                                    [ Color (animationColor n) $ rectangleSolid (fromIntegral w) (fromIntegral h)
                                                    , Color white $ translate (-100) 0 $ scale 0.3 0.3 $ Text $ show l ]
                                                    where animationColor n
                                                            | n > 25 = makeColor 0 0 0 (0.04*(50-n))
                                                            | otherwise = makeColor 0 0 0 (0.04*n)
