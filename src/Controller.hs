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
    -- let gstate = gstate {player = updatedPlayer} -- incase we want to make things more readable, this is also a way of jotting things down
    -- spawn new enemies
    random1 <- randomRIO (1, 5) :: IO Int
    random2 <- randomRIO (1, 100) :: IO Float
    let randomY = random2
    let swarm = Swarm 3 (Pt 50 randomY) 3
    let newEnemies = swarm


    -- let updatedEnemies = mapMaybe (hitBoxCheck (bullets (player gstate))) (enemies gstate) -- update enemy?
    -- return (gstate { elapsedTime = elapsedTime gstate + secs, player = updatedPlayer, enemies = updatedEnemies})

    -- check if any bullet hits an enemy and moves the bullet
    let updatedBullets = mapMaybe (\bullet -> bulletHit bullet (enemies gstate)) (map moveBullet bts)
    let updatedEnemies = mapMaybe (killEnemy (map moveBullet bts)) (map moveEnemy (enemies gstate))
    let updatedPlayer = (player gstate) {bullets = updatedBullets}

    let updatedScore = calculateScore updatedEnemies (enemies gstate)

    return (gstate { elapsedTime = elapsedTime gstate + secs, player = updatedPlayer, enemies = newEnemies : updatedEnemies, score = updatedScore })
  where
    (P p w v l bts) = player gstate


-- spawnEnemy :: IO Int -> IO Float -> Maybe Enemy
-- spawnEnemy r1 ry
--   | r1 == swarmFreq = 

-- update the score by checking which enemies have been killed, might run into trouble when deleting enemies after going off screen. 
-- May need to do that deletion at the very end of the step, after calcing score.
calculateScore :: [Enemy] -> [Enemy] -> Score
calculateScore l1 l2 = calcScore (filter (`notElem` l1) l2)

calcScore :: [Enemy] -> Score
calcScore [] = 0
calcScore (e:es) = 
    points + calcScore es
  where
    points = case e of
                Swarm {} -> 5
                Turret {} -> 10
                Worm {} -> 15
                Boss {} -> 50

-- define the movements of the bullets of the player
moveBullet :: Bullet -> Bullet
moveBullet (Pea (Pt x y)) = Pea (Pt (x + 5) y)
moveBullet (Rocket (Pt x y)) = Rocket (Pt (x + 7) y)
moveBullet (Laserbeam (Pt x y)) = Laserbeam (Pt (x + 15) y) -- schiet elke frame een kogel



-- define movement of the enemies
moveEnemy :: Enemy -> Enemy
moveEnemy (Swarm h (Pt x y) s) = Swarm h (Pt (x - 3) y) s
moveEnemy (Turret h (Pt x y) s) = Turret h (Pt (x - 3) y) s
moveEnemy (Worm h (Pt x y) s) = Worm h (Pt (x - 3) y) s
moveEnemy (Boss h (Pt x y) s) = Boss h (Pt (x - 3) y) s

-- | Check if any of the bullets hit an enemy and destroy the bullet, do nothing with the enemy
bulletHit :: Bullet -> [Enemy] -> Maybe Bullet
bulletHit bullet [] = Just bullet -- keep bullet if it hits no enemies
bulletHit bullet (enemy : enemies)
  | uncurry (ptInSquare p1) ei = Nothing -- discard the bullet if it is in the hitbox of the enemy
  | otherwise = bulletHit bullet enemies -- otherwise recurse
  where
    p1 = bulletPos bullet
    ei = enemyPos enemy

enemyPos :: Enemy -> (Pos, Size)
enemyPos (Swarm _ p s) = (p, s)
enemyPos (Turret _ p s) = (p, s)
enemyPos (Worm _ p s) = (p, s)
enemyPos (Boss _ p s) = (p, s)

-- | check if any bullets hit an enemy and degrade its health
killEnemy :: [Bullet] -> Enemy -> Maybe Enemy
killEnemy [] e = Just e
killEnemy (b:bts) enemy@(Swarm h s p2)
  | uncurry (ptInSquare p1) ei && h - dmg <= 0 = Nothing
  | uncurry (ptInSquare p1) ei && h - dmg > 0 = killEnemy bts (Swarm (h - dmg) s p2)
  | otherwise = killEnemy bts enemy
  where
    p1 = bulletPos b
    ei = enemyPos enemy
    dmg = case show b of
            "Pea" -> 1 -- is er een manier waarop ik dit kan global kan definieren? 
            "Rocket" -> 5
            "Laserbeam"  -> 10 -- verlagen

bulletPos :: Bullet -> Pos
bulletPos (Pea p) = p
bulletPos (Rocket p) = p 
bulletPos (Laserbeam p) = p

-- check if a point is in a square for bullets, may need altering for bigger hitboxes for bullets
ptInSquare :: Pos -> Pos -> Size -> Bool
ptInSquare (Pt x y) (Pt xe ye) s = x <= xe + s && x >= xe && y <= ye + s && y >= ye



-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- update player

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = case c of
      'w' -> gstate { infoToShow = ShowAChar 'w', player = P (Pt x (y + 10)) we vel l b }
      'a' -> gstate { infoToShow = ShowAChar 'a', player = P (Pt (x - 10) y) we vel l b }
      's' -> gstate { infoToShow = ShowAChar 's', player = P (Pt x (y - 10)) we vel l b }
      'd' -> gstate { infoToShow = ShowAChar 'd', player = P (Pt (x + 10) y) we vel l b }
    where
      (P (Pt x y) we vel l b) = player gstate

-- | Shoot bullets!
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
  = gstate { infoToShow = ShowAChar 'B', player = P (Pt x y) we vel l (b ++ [bullet]) } -- shoot
    where
      (P (Pt x y) we vel l b) = player gstate
      bullet = getBullet we (Pt x y)


inputKey (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = gstate { infoToShow = ShowAString "SW", player = P (Pt x y) we vel l b } -- shoot
    where
      (P (Pt x y) we vel l b) = player gstate
      bullet = getBullet we (Pt x y)

inputKey _ gstate = gstate -- Otherwise keep the same


getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter = Pea
getBullet Launcher = Rocket
getBullet Laser = Laserbeam

switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter