-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import System.Random
import Data.Maybe
import Data.Set as S hiding (map, filter)
-- import System.Exit (exitSuccess)

-- | Handle one iteration of the game
-- | Move background on the screen
-- | Enemies spawn, move here and attack here
-- | Spawn enemies here based on a frequency which is correlated to the score of the player
step :: Float -> GameState -> IO GameState
step secs gstate = 
  do 
    -- let gstate = gstate {player = updatedPlayer} -- incase we want to make things more readable, this is also a way of jotting things down
    -- spawn new enemies
    screenSize <- getScreenSize
    let xScreen = fst screenSize
    let yScreen = snd screenSize

    random1 <- randomRIO (1, 5) :: IO Int
    random2 <- randomRIO (- fromIntegral yScreen, fromIntegral yScreen) :: IO Float
    let randomY = random2
    -- Spawner
    -- unzip something
    let res = unzip (spawner (timer gstate) secs (Pt (fromIntegral (xScreen `div` 2)) randomY))
    let newTimeFreq = fst res
    let newEnemies = catMaybes (snd res)


    -- check if any bullet hits an enemy and moves the bullet
    let updatedBullets = mapMaybe (\bullet -> bulletHit bullet (enemies gstate)) (map moveBullet bts)
    let updatedPlayer = (player gstate) {bullets = updatedBullets}
    let movementUpdatedPlayer = movementHandler (S.toList (keys gstate)) updatedPlayer
    let bulletUpdatedPlayer = bulletHandler (keys gstate) secs movementUpdatedPlayer

    let updatedEnemies = mapMaybe (killEnemy (map moveBullet bts)) (map moveEnemy (enemies gstate))
    let updatedScore = calculateScore updatedEnemies (enemies gstate)

    return (gstate { infoToShow = ShowANumber 0
    , elapsedTime = elapsedTime gstate + secs
    , player = bulletUpdatedPlayer
    , enemies = updatedEnemies ++ newEnemies
    , timer = newTimeFreq
    , score = updatedScore })
  where
    (P p w v l bts) = player gstate


offscreenRemover :: [Bullet] -> (Int, Int) -> [Bullet]
offscreenRemover [] _ = []
offscreenRemover (b:bts) (x, y) = undefined


spawner :: [TimerFreq] -> Float -> Pos -> [(TimerFreq, Maybe Enemy)]
spawner [] _ _ = []
spawner ((T name time freq):r) secs p 
    | time >= freq = ((T name 0 freq), Just enemy) : spawner r secs p
    | otherwise = ((T name (time + secs) freq), Nothing) : spawner r secs p
    where
      enemy = case name of
        "Swarm" -> Swarm 3 p 10
        "Turret" -> Turret 10000 (Pt 200 0) 30 -- dont know what the position of this needs to be
        "Worm" -> Worm 5 p 30


movementHandler :: [Char] -> Player -> Player
movementHandler [] p = p
movementHandler (c:chars) p = movementHandler chars newPlayer  
  where
    (P (Pt x y) we vel l b) = p
    newPlayer = case c of
      'w' -> P (Pt x (y + 10)) we vel l b
      'a' -> P (Pt (x - 10) y) we vel l b 
      's' -> P (Pt x (y - 10)) we vel l b
      'd' -> P (Pt (x + 10) y) we vel l b
      -- '.' -> P (Pt x y) we vel l (getBullet we (Pt x y) : b)
      _ -> p -- if it gets a key it doesnt understand ignore it (redundant if well implemented)
bulletHandler :: Set Char -> Float -> Player -> Player
bulletHandler s t p
    | S.member '.' s = P (Pt x y) we vel l (getBullet we (Pt x y) : b)
    | otherwise = p
  where
    (P (Pt x y) we vel l b) = p
    -- timer = case we of
    --   Peashooter -> peashooterTimer
    --   Launcher -> launcherTimer
    --   Laser -> laserTimer


-- update the score by checking which enemies have been killed, might run into trouble when deleting enemies after going off screen. 
-- May need to do that deletion at the very end of the step, after calcing score.
calculateScore :: [Enemy] -> [Enemy] -> Score
calculateScore l1 l2 = calcScore (filter (`notElem` l2) l1) -- use Prelude since not a set
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
  | ptInSquare p1 p s = Nothing -- discard the bullet if it is in the hitbox of the enemy
  | otherwise = bulletHit bullet enemies -- otherwise recurse
  where
    p1 = bulletPos bullet
    (h, p, s) = enemyInf enemy

enemyInf :: Enemy -> (Health, Pos, Size)
enemyInf (Swarm h p s) = (h, p, s)
enemyInf (Turret h p s) = (h, p, s)
enemyInf (Worm h p s) = (h, p, s)
enemyInf (Boss h p s) = (h, p, s)

-- | check if any bullets hit an enemy and degrade its health
killEnemy :: [Bullet] -> Enemy -> Maybe Enemy
killEnemy [] e = Just e
killEnemy (b:bts) enemy
  | ptInSquare p1 p s && h - dmg <= 0 = Nothing
  | ptInSquare p1 p s && h - dmg > 0 = killEnemy bts (damageEnemy enemy dmg)
  | otherwise = killEnemy bts enemy
  where
    p1 = bulletPos b
    (h, p, s) = enemyInf enemy
    dmg = case b of
            Pea {} -> 1 -- is er een manier waarop ik dit kan global kan definieren? 
            Rocket {} -> 5
            Laserbeam {}  -> 10 -- verlagen

damageEnemy :: Enemy -> Damage -> Enemy
damageEnemy e d = case e of
  Swarm h p s -> Swarm (h - d) p s
  Worm h p s -> Worm (h - d) p s 
  Turret h p s -> Turret (h - d) p s 
  Boss h p s -> Boss (h - d) p s 

bulletPos :: Bullet -> Pos
bulletPos (Pea p) = p
bulletPos (Rocket p) = p 
bulletPos (Laserbeam p) = p

-- check if a point is in a square for bullets, may need altering for bigger hitboxes for bullets
ptInSquare :: Pos -> Pos -> Size -> Bool
ptInSquare (Pt x y) (Pt xe ye) s = x <= xe + s && x >= xe && y <= ye + s && y >= ye

getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter = Pea
getBullet Launcher = Rocket
getBullet Laser = Laserbeam

switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate = gstate {keys = S.insert c (keys gstate)}
inputKey (EventKey (Char c) Up _ _) gstate = gstate {keys = S.delete c (keys gstate)}
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = case c of
--       'w' -> gstate { infoToShow = ShowAChar 'w', player = P (Pt x (y + 10)) we vel l b }
--       'a' -> gstate { infoToShow = ShowAChar 'a', player = P (Pt (x - 10) y) we vel l b }
--       's' -> gstate { infoToShow = ShowAChar 's', player = P (Pt x (y - 10)) we vel l b }
--       'd' -> gstate { infoToShow = ShowAChar 'd', player = P (Pt (x + 10) y) we vel l b }
    -- where
    --   (P (Pt x y) we vel l b) = player gstate

-- | Shoot bullets!
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate { keys = S.insert '.' (keys gstate)}
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = gstate { keys = S.delete '.' (keys gstate)}

  -- = gstate { infoToShow = ShowAChar 'B', player = P (Pt x y) we vel l (b ++ [bullet]) } -- shoot
  --   where
  --     (P (Pt x y) we vel l b) = player gstate
  --     bullet = getBullet we (Pt x y)
inputKey (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = gstate { infoToShow = ShowAString "SW", player = P (Pt x y) we vel l b } -- switch weapon
    where
      (P (Pt x y) we vel l b) = player gstate
      bullet = getBullet we (Pt x y)

inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate = error "Game exited"
inputKey _ gstate = gstate -- Otherwise keep the same
