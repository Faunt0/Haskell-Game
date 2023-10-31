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
import Offscreen


-- TODOS
-- maybe make more packages to have a cleaner file
-- collision damage
-- Misschien een sniper maken of burst in meerdere richtingen
-- boosts/health maken
-- artwork
-- background elements
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
    let res = unzip (spawner (timer gstate) secs screenSize (Pt (fromIntegral (xScreen `div` 2)) randomY))
    let newTimeFreq = fst res
    let newEnemies = catMaybes (snd res)


    -- check if any bullet hits an enemy and moves the bullet
    let movedPlayer = movementHandler (S.toList (keys gstate)) (screenSize) (player gstate) -- move the player
    let movedBullets = mapMaybe (\bullet -> bulletHit bullet (enemies gstate)) (map moveBullet bts) -- move the bullets and check if they should be discarded
    let removeOffscreenBullets = bulletOffscreen movedBullets screenSize
    let bulletHandlerRes = bulletHandler (keys gstate) secs movedPlayer -- add new bullets based on the weapon and rate (and amount maybe)
    let newBullets = snd bulletHandlerRes
    let newPlayerTimer = fst bulletHandlerRes
    let updatedPlayer = movedPlayer {bullets = newBullets ++ removeOffscreenBullets, playerTimer = newPlayerTimer} -- update them for the player


    -- Handle the enemies
    let moveEnemies = map (moveEnemy) (enemies gstate) -- move the enemies
    let updatedEnemies = map (damageEnemy (map moveBullet bts)) moveEnemies -- damage the enemies
    let splitEnemiesRes = splitEnemies updatedEnemies [] [] -- start with 0 alive/dead enemies
    let removeOffscreenEnemies = enemyOffscreen (fst splitEnemiesRes) screenSize -- remove offscreen alive enemies
    let enemyBts = unzip (map (enemyFire updatedPlayer secs) removeOffscreenEnemies)
    let moveEnemyBullets = map moveBullet (enemyBullets gstate)
    let updatedScore = score gstate + calcScore (snd splitEnemiesRes)




    return (gstate { infoToShow = ShowANumber 0
    , elapsedTime = elapsedTime gstate + secs
    , player = updatedPlayer
    , enemies = fst enemyBts ++ newEnemies
    , enemyBullets = moveEnemyBullets ++ flatten (snd enemyBts)
    , timer = newTimeFreq
    , score = updatedScore })
  where
    (P p w v l (t, f) bts) = player gstate

flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (h:t) = h ++ flatten t

-- laser dynamics
bulletHandler :: Set Char -> Float -> Player -> ((Time, Freq), [Bullet])
bulletHandler set secs p
    | S.member '.' set && t >= f = ((0, f), [getBullet we (Pt x y)])
    | otherwise = ((t + secs, f), [])
  where
    (Pt x y) = position p
    we = weapon p
    (t, f) = playerTimer p

-- beetje randomizen
enemyFire :: Player -> Float -> Enemy -> (Enemy, [Bullet])
enemyFire player secs e
  | t >= f = (e {enemyRate = (0, f)}, b)
  | otherwise = (e {enemyRate = (t + secs, f)}, [])
  where
    (t, f) = enemyRate e
    ePos@(Pt xe ye) = enemyPosition e
    (Pt xp yp) = position player
    xdif = xp - xe
    ydif = yp - ye
    d@(dx, dy) = (4 * (xdif / (xdif + ydif)), 4 * (ydif / (xdif + ydif))) -- let op dat je niet door 0 deelt als je op de vijand staat
    -- incorporate the direction of the bullets based on the position of the player, maybe bullets taht move like snakes?
    b = case enemySpecies e of
        Swarm -> [Bullet Pea ePos 5 (dx, dy) 5]
        Worm -> [] 
        Turret -> [Bullet Pea ePos 5 (dx, dy) 5, Bullet Pea ePos 5 (-dx, dy) 5]
        Boss -> [Bullet Rocket ePos 5 (dx, dy) 5]
        -- Swarm -> [Vomit, Vomit]
        -- Worm -> []
        -- Turret -> [Vermin]
        -- Boss -> [Globs]


-- define the movements of the bullets of the player
moveBullet :: Bullet -> Bullet
moveBullet b = b {bulletPosition = Pt (x + fst (bulletSpeed b)) (y + snd (bulletSpeed b))}
    where
      (Pt x y) = bulletPosition b -- not sure that this is the right way, maybe different bullets move differently


-- define movement of the enemies this needs to be more complex. think sine waves or diagonals or something alike
moveEnemy :: Enemy -> Enemy
moveEnemy e = e {enemyPosition = Pt (x - xdif) (y- ydif)}
  where
    (Pt x y) = enemyPosition e
    f :: Float -> Float
    f a = 1 * sin (1/(100 * 2*pi) * (a - xdif))

    (xdif, ydif) = case enemySpecies e of
      Swarm -> (3, f x)
      Turret -> (3, 0)
      Worm -> (3, f x)
      Boss -> (3, f x) -- deze beweegt toch niet?



-- | check if any bullets hit an enemy and degrade its health
damageEnemy :: [Bullet] -> Enemy -> Enemy
damageEnemy [] e = e
damageEnemy (b:bts) e
  | ptInSquare b e = damageEnemy bts (e { enemyHealth = enemyHealth e - bulletDamage b }) -- werkt dit zo?
  | otherwise = damageEnemy bts e


-- split the list of enemies into a list of alive enemies and dead ones
splitEnemies :: [Enemy] -> [Enemy] -> [Enemy] -> ([Enemy], [Enemy]) -- maybe define new types to distinguish better what each list represents
splitEnemies [] alive dead = (alive, dead)
splitEnemies (e:es) alive dead
    | enemyHealth e <= 0 = splitEnemies es alive (e:dead)
    | otherwise = splitEnemies es (e:alive) dead


spawner :: [TimerFreq] -> Float -> (Int, Int) -> Pos -> [(TimerFreq, Maybe Enemy)]
spawner [] _ _ _ = []
spawner ((T name time freq):r) secs screen@(xScreen, yScreen) p 
    | time >= freq = ((T name 0 freq), Just enemy) : spawner r secs screen p
    | otherwise = ((T name (time + secs) freq), Nothing) : spawner r secs screen p
    where
      enemy = case name of
        "Swarm" -> Enemy Swarm 3 p 20 (0, 1)
        "Turret" -> Enemy Turret 100000000 (Pt (fromIntegral (xScreen `div` 2)) (0 - fromIntegral (yScreen `div` 2))) 30 (0, 1)
        "Worm" -> Enemy Worm 5 p 30 (0, 3)



calcScore :: [Enemy] -> Score
calcScore [] = 0
calcScore (e:es)
  | enemyHealth e <= 0 = points + calcScore es
  | otherwise = calcScore es
  where
    points = case enemySpecies e of
              Swarm -> 5 -- how should I define this properly? things like globally defined variables in packages
              Turret -> 10
              Worm -> 15
              Boss -> 50



movementHandler :: [Char] -> (Int, Int) -> Player -> Player
movementHandler [] s p = p
movementHandler (c:chars) s@(xScreen, yScreen) p = movementHandler chars s newPlayer
  where
    (Pt x y) = position p
    newPlayer = case c of
      'w' -> p {position = (Pt x (y + 8))}
      'a' -> p {position = (Pt (x - 8) y)}
      's' -> p {position = (Pt x (y - 8))}
      'd' -> p {position = (Pt (x + 8) y)}
      _ -> p -- if it gets a key it doesnt understand ignore it (redundant if well implemented)

-- | Check if any of the bullets hit an enemy and destroy the bullet, do nothing with the enemy
bulletHit :: Bullet -> [Enemy] -> Maybe Bullet
bulletHit bullet [] = Just bullet -- keep bullet if it hits no enemies
bulletHit bullet (enemy : enemies)
  | ptInSquare bullet enemy = Nothing -- discard the bullet if it is in the hitbox of the enemy
  | otherwise = bulletHit bullet enemies -- otherwise recurse

-- check if a point is in a square for bullets, may need altering for bigger hitboxes for bullets, do we just define the size of the bullets? or base it off of the weapon/bullet used
ptInSquare :: Bullet -> Enemy -> Bool
ptInSquare b e = xb <= xe + s + margin && xe - margin <= xb && yb <= ye + s + margin && ye - margin <= yb
    where
      (Pt xb yb) = bulletPosition b
      s = enemySize e
      (Pt xe ye) = enemyPosition e
      margin = bulletSize b



getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter p = Bullet Pea p 5 (8, 0) 5
getBullet Launcher p = Bullet Rocket p 10 (8, 0) 10
getBullet Laser p = Bullet Laserbeam p 10 (8, 0) 40      -- alter the way this works, meaning also having to alter bulletmovements

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
