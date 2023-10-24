-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import System.Random
import Data.Maybe

-- | Handle one iteration of the game
-- | Move background on the screen
-- | Enemies spawn, move here and attack here
-- | Spawn enemies here based on a frequency which is correlated to the score of the player
step :: Float -> GameState -> IO GameState
step secs gstate = 
  do 
    let updatedPlayer = (player gstate) { bullets = updateBullets b } -- update the bullets of the player
    -- let gstate = gstate {player = updatedPlayer} -- incase we want to make things more readable, this is also a way of jotting things down
    randomNumber <- randomIO :: IO Int
    let updatedEnemies = mapMaybe (hitBoxCheck (bullets (player gstate))) (enemies gstate)
    return (gstate { elapsedTime = elapsedTime gstate + secs, player = updatedPlayer, enemies = updatedEnemies})
  where
    (P p w v l b) = player gstate

-- | move the bullets
updateBullets :: [Bullet] -> [Bullet]
updateBullets [] = []
updateBullets [Pt x y] = [Pt (x+5) y]
updateBullets ((Pt x y):xs) = Pt (x+5) y : updateBullets xs

-- | Check if any of the bullets hit an enemy and destroy the enemy
-- hitBoxCheck :: [Bullet] -> Enemy -> Maybe Enemy
hitBoxCheck :: [Bullet] -> Swarm -> Maybe Swarm
hitBoxCheck [] e = Just e
hitBoxCheck [Pt x y] e@(Swarm h s (Pt xe ye))
  | x <= xe + s && x >= xe && y <= ye + s && y >= ye = Nothing -- kill the enemy directly, regardless of health
  | otherwise = Just e
hitBoxCheck ((Pt x y):pts) e@(Swarm h s (Pt xe ye))
  | x <= xe + s && x >= xe && y <= ye + s && y >= ye = Nothing -- kill the enemy directly, regardless of health
  | otherwise = hitBoxCheck pts e

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
  = case c of
      'w' -> gstate { infoToShow = ShowAChar 'w', player = P (Pt x (y + 10)) we vel l b }
      'a' -> gstate { infoToShow = ShowAChar 'a', player = P (Pt (x - 10) y) we vel l b }
      's' -> gstate { infoToShow = ShowAChar 's', player = P (Pt x (y - 10)) we vel l b }
      'd' -> gstate { infoToShow = ShowAChar 'd', player = P (Pt (x + 10) y) we vel l b }
    where
      (P (Pt x y) we vel l b) = player gstate

-- | Shoot bullets!
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
  = gstate { infoToShow = ShowAChar 'B', player = P (Pt x y) we vel l (b ++ [Pt x y]) } -- shoot
    where
      (P (Pt x y) we vel l b) = player gstate

inputKey _ gstate = gstate -- Otherwise keep the same