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



-- TODOS
-- maybe make more packages to have a cleaner file
-- collision damage
-- Misschien een sniper maken of burst in meerdere richtingen
-- boosts/health maken
-- artwork
-- background elements
-- misschien moet er rekening gehouden worden met
-- misschien werken met ammo en reload systemen zodat je niet gewoon je knoppen kunt blijven spammen?
-- misschien iets met dat je een moederschip moet beschermen wat damage krijgt van collissions met enemies
-- misschien enemies die stil staan op gegeven momenten en gewoon schieten
-- misschien een soort backup players die achter de Player schieten.


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
    let res = unzip (spawner (timer gstate) secs (Pt (fromIntegral (xScreen `div` 2)) randomY))
    let newTimeFreq = fst res
    let newEnemies = catMaybes (snd res)


    -- check if any bullet hits an enemy and moves the bullet
    let movedPlayer = movementHandler (S.toList (keys gstate)) (screenSize) (player gstate) -- move the player
    let movedBullets = mapMaybe (\bullet -> bulletHit bullet (enemies gstate)) (map moveBullet bts) -- move the bullets and check if they should be discarded
    let removeOffscreenBullets = bulletOffscreen movedBullets screenSize
    let bulletHandlerRes = bulletHandler (keys gstate) (elapsedTime gstate + secs) movedPlayer -- add new bullets based on the weapon and rate (and amount maybe)
    let newBullets = snd bulletHandlerRes
    let newPlayerTimer = fst bulletHandlerRes
    let updatedPlayer = movedPlayer {bullets = newBullets ++ removeOffscreenBullets, playerTimer = newPlayerTimer} -- update them for the player


    -- Handle the enemies
    let moveEnemies = map moveEnemy (enemies gstate) -- move the enemies
    let updatedEnemies = map (damageEnemy (map moveBullet bts)) moveEnemies -- damage the enemies
    let splitEnemiesRes = splitEnemies updatedEnemies [] [] -- start with 0 alive/dead enemies
    let removeOffscreenEnemies = enemyOffscreen (fst splitEnemiesRes) screenSize -- remove offscreen alive enemies

    let updatedScore = score gstate + calcScore (snd splitEnemiesRes)



    return (gstate { infoToShow = ShowANumber 0
    , elapsedTime = elapsedTime gstate + secs
    , player = updatedPlayer
    , enemies = removeOffscreenEnemies ++ newEnemies
    , timer = newTimeFreq
    , score = updatedScore })
  where
    (P p w v l (t, f) bts) = player gstate


-- need to update to incorporate the rate of fire and also account for laser mechanics, a beam/ray of bullets continuously shooting from the player at the same y as the player
bulletHandler :: Set Char -> Float -> Player -> ((Time, Freq), [Bullet])
bulletHandler set secs p
    | S.member '.' set && t > f = ((0, f), [getBullet we (Pt x y)])
    | otherwise = ((t + secs, f), [])
  where
    (P (Pt x y) we vel l (t, f) b) = p


-- | check if any bullets hit an enemy and degrade its health
-- May need to be altered to return the enemy with negative health to then calculate the score more easily when dealing with spawners and offscreen deletions
damageEnemy :: [Bullet] -> Enemy -> Enemy
damageEnemy [] e = e
damageEnemy (b:bts) e
  | ptInSquare b e = damageEnemy bts enemy
  | otherwise = damageEnemy bts e
  where
    p1 = bulletPos b
    (h, p, s) = enemyInf enemy
    dmg = case b of
            Pea {} -> 1 -- is er een manier waarop ik dit kan global kan definieren? ja! 
            Rocket {} -> 5
            Laserbeam {}  -> 10 -- verlagen
    enemy = case e of
          Swarm h p s be -> Swarm (h - dmg) p s be
          Worm h p s be -> Worm (h - dmg) p s be
          Turret h p s be -> Turret (h - dmg) p s be 
          Boss h p s be -> Boss (h - dmg) p s be


-- split the list of enemies into a list of alive enemies and dead ones
splitEnemies :: [Enemy] -> [Enemy] -> [Enemy] -> ([Enemy], [Enemy]) -- maybe define new types to distinguish better what each list represents
splitEnemies [] alive dead = (alive, dead)
splitEnemies (e:es) alive dead
    | h <= 0 = splitEnemies es alive (e:dead)
    | otherwise = splitEnemies es (e:alive) dead
    where
      (h, p, s) = enemyInf e


-- may want to add a margin so the enemy does not immediately despawn the second it hits the edge of the screen
bulletOffscreen :: [Bullet] -> (Int, Int) -> [Bullet]
bulletOffscreen [] _ = []
bulletOffscreen (b:bts) (x, y)
    | fromIntegral (x `div` 2) - 100 <= bx = bulletOffscreen bts (x, y)
    | otherwise = b : bulletOffscreen bts (x, y)
    where
      (Pt bx by) = bulletPos b
enemyOffscreen :: [Enemy] -> (Int, Int) -> [Enemy]
enemyOffscreen [] _ = []
enemyOffscreen (b:bts) (x, y)
    | fromIntegral (x `div` 2) <= ex = enemyOffscreen bts (x, y)
    | otherwise = b : enemyOffscreen bts (x, y)
    where
      (_, Pt ex ey, _) = enemyInf b


spawner :: [TimerFreq] -> Float -> Pos -> [(TimerFreq, Maybe Enemy)]
spawner [] _ _ = []
spawner ((T name time freq):r) secs p 
    | time >= freq = ((T name 0 freq), Just enemy) : spawner r secs p
    | otherwise = ((T name (time + secs) freq), Nothing) : spawner r secs p
    where
      enemy = case name of
        "Swarm" -> Swarm 3 p 20 []
        "Turret" -> Turret 10000 (Pt 200 0) 30 [] -- dont know what the position of this needs to be
        "Worm" -> Worm 5 p 30 []


calcScore :: [Enemy] -> Score
calcScore [] = 0
calcScore (e:es)
  | h <= 0 = points + calcScore es
  | otherwise = calcScore es
  where
    (h, points) = case e of
              Swarm h p s b -> (h, 5) -- how should I define this properly? things like globally defined variables in packages
              Turret h p s be -> (h, 10)
              Worm h p s be -> (h, 15)
              Boss h p s be -> (h, 50)


-- define the movements of the bullets of the player
moveBullet :: Bullet -> Bullet
moveBullet (Pea (Pt x y)) = Pea (Pt (x + peashooterSpeed) y)
moveBullet (Rocket (Pt x y)) = Rocket (Pt (x + rocketSpeed) y)
moveBullet (Laserbeam (Pt x y)) = Laserbeam (Pt (x + 15) y) -- schiet elke frame een kogel



-- define movement of the enemies this needs to be more complex. think sine waves or diagonals or something alike
moveEnemy :: Enemy -> Enemy
moveEnemy (Swarm h (Pt x y) s be) = Swarm h (Pt (x - 3) y) s be
moveEnemy (Turret h (Pt x y) s be) = Turret h (Pt (x - 3) y) s be
moveEnemy (Worm h (Pt x y) s be) = Worm h (Pt (x - 3) y) s be
moveEnemy (Boss h (Pt x y) s be) = Boss h (Pt (x - 3) y) s be

movementHandler :: [Char] -> (Int, Int) -> Player -> Player
movementHandler [] s p = p
movementHandler (c:chars) s@(xScreen, yScreen) p = movementHandler chars s newPlayer
  where
    (P (Pt x y) we vel l t b) = p
    newPlayer = case c of
      'w' -> P (Pt x (y + 8)) we vel l t b
      'a' -> P (Pt (x - 8) y) we vel l t b 
      's' -> P (Pt x (y - 8)) we vel l t b
      'd' -> P (Pt (x + 8) y) we vel l t b
      _ -> p -- if it gets a key it doesnt understand ignore it (redundant if well implemented)

-- | Check if any of the bullets hit an enemy and destroy the bullet, do nothing with the enemy
bulletHit :: Bullet -> [Enemy] -> Maybe Bullet
bulletHit bullet [] = Just bullet -- keep bullet if it hits no enemies
bulletHit bullet (enemy : enemies)
  | ptInSquare bullet enemy = Nothing -- discard the bullet if it is in the hitbox of the enemy
  | otherwise = bulletHit bullet enemies -- otherwise recurse
  where
    p1 = bulletPos bullet
    (h, p, s) = enemyInf enemy
-- check if a point is in a square for bullets, may need altering for bigger hitboxes for bullets, do we just define the size of the bullets? or base it off of the weapon/bullet used
ptInSquare :: Bullet -> Enemy -> Bool
ptInSquare b e = xb <= xe + s + margin && xe - margin <= xb && yb <= ye + s + margin && ye - margin <= yb
    where
      (Pt xb yb) = bulletPos b
      (h, (Pt xe ye), s) = enemyInf e
      margin = case b of
        Pea {} -> 5 -- half of the size of the circle we use to view it, radius v diameter things
        Rocket {} -> 10
        Laserbeam {} -> 5 -- this is all different since its basically a bunch of bullets



enemyInf :: Enemy -> (Health, Pos, Size)
enemyInf (Swarm h p s be) = (h, p, s)
enemyInf (Turret h p s be) = (h, p, s)
enemyInf (Worm h p s be) = (h, p, s)
enemyInf (Boss h p s be) = (h, p, s)

bulletPos :: Bullet -> Pos
bulletPos (Pea p) = p
bulletPos (Rocket p) = p
bulletPos (Laserbeam p) = p

getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter = Pea
getBullet Launcher = Rocket
getBullet Laser = Laserbeam -- alter the way this works, meaning also having to alter bulletmovements

switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Handle movements which can be held
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate 
  | c == 'w' || c=='a' || c=='s' || c=='d' = gstate {keys = S.insert c (keys gstate)} -- may need to specify which keys are possible
  | otherwise = gstate
inputKey (EventKey (Char c) Up _ _) gstate = gstate {keys = S.delete c (keys gstate)}

-- | Shoot bullets!
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate { keys = S.insert '.' (keys gstate)}
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = gstate { keys = S.delete '.' (keys gstate)}

-- | Switch weapon when possible
inputKey (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = gstate { infoToShow = ShowAString "SW", player = P (Pt x y) newWe vel l (0, newF) b } -- switch weapon
    where
      (P (Pt x y) we vel l (t, f) b) = player gstate
      newWe = switchWeapon we
      newF = case newWe of
        Peashooter -> 0.5
        Launcher -> 1
        Laser -> 0.1 -- lastig dit zeg tering

inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate = error "Game exited"
inputKey _ gstate = gstate -- Otherwise keep the same
