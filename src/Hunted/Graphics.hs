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
    backgroundTexture <- loadBMP "images/background-tile.bmp"
    return Textures { background = backgroundTexture
                    , player = playerTextureSet
                    , monsterWalking = monsterWalkingSet
                    , monsterHunting = monsterHuntingSet }

loadAnims :: String -> String -> String -> IO [Picture]
loadAnims path1 path2 path3 = fun <$> loadBMP path1 <*> loadBMP path2 <*> loadBMP path3
                              where fun a b c = [a,b,c]



{- again, need to export gloss internal state for this signature, pull request required
renderFrame :: Window
               -> gloss-rendering-1.9.2.1:Graphics.Gloss.Internals.Rendering.State.State
               -> Textures
               -> (Int, Int)
               -> (Float, Float)
               -> RenderState
               -> IO ()
-}
renderFrame window glossState textures dimensions (worldWidth, worldHeight) (RenderState (Player _ playerDir) monster gameOver viewport bolts) = do
   displayPicture dimensions black glossState (viewPortScale viewport) $ 
     Pictures $ gameOngoing gameOver $
                             [ uncurry translate (viewPortTranslate viewport) $ tiledBackground (background textures) worldWidth worldHeight
                             , renderPlayer playerDir (player textures)
                             , uncurry translate (viewPortTranslate viewport) $ renderMonster monster (monsterWalking textures) (monsterHunting textures)
                             , uncurry translate (viewPortTranslate viewport) $ renderHealthBar monster ]
                              ++ (map (uncurry translate (viewPortTranslate viewport) . renderBolt) bolts)
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
gameOngoing :: Maybe Ending -> [Picture] -> [Picture]
gameOngoing (Just Lose) pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
gameOngoing (Just Win) pics =  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "You win!"]
gameOngoing Nothing pics =  pics
