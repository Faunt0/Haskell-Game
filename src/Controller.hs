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
import Offscreen
import GameMechanics
import System.Directory (getDirectoryContents)
import GameMechanics (mountainSize)



-- | Handle one iteration of the game
-- | Move background on the screen
-- | Enemies spawn, move here and attack here
-- | Spawn enemies here based on a frequency which is correlated to the score of the player
step :: Float -> GameState -> IO GameState
step secs gstate
  | status gstate == StartScreen = return gstate
  | status gstate == Pause = return gstate 
  | status gstate == GameOver = return gstate 
  | otherwise =
  do
    -- spawn new enemies
    screenSize <- getScreenSize
    let xScreen = fst screenSize
    let yScreen = snd screenSize
    let margin = 0

    random1 <- randomRIO (1, 5) :: IO Int
    random2 <- randomRIO (- fromIntegral (yScreen `div` 2), fromIntegral (yScreen `div` 2)) :: IO Float
    let randomY = random2


    -- Spawner for both enemies and background elements
    let res = unzip (spawner (timer gstate) secs screenSize (Pt (fromIntegral (xScreen `div` 2) + xmargin) randomY))
    let newTimeFreq = fst res
    let newEntities = catMaybes (snd res)
    let newEnemies = filter (\e -> entityType e `elem` [Swarm, Turret, Brute]) newEntities
    let newBg = filter (\e -> entityType e `elem` [Cloud, Planet, Mountain]) newEntities


    let movedPlayer = movementHandler (keys gstate) screenSize (player gstate)

    
    let movedBullets = map moveEntity (bullets movedPlayer)
    let removeOffscreenBullets = entityOffscreen movedBullets screenSize 
    let bulletHandlerRes = bulletHandler (keys gstate) secs movedPlayer 
    let updatedPlayer = movedPlayer {bullets = removeOffscreenBullets ++ snd bulletHandlerRes, rate = fst bulletHandlerRes}


    -- Handle damage between entities and the player
    let movedEntities = map moveEntity (enemies gstate)
    let onScreenEntities = entityOffscreen movedEntities screenSize

    let collisions = collissionCheck onScreenEntities updatedPlayer 

    let splitAliveDeadRes = splitAliveDead (fst collisions)


    -- Split bullets for the player
    let splitPlayerBullets = splitAliveDead (bullets (snd collisions))
    -- Only keep the alive bullets
    let updatedPlayer2 = (snd collisions) {bullets = fst splitPlayerBullets} 

    -- Spawn explosions when rockets and worms die
    let explosions = necroSpawner (snd splitPlayerBullets ++ snd splitAliveDeadRes) secs
    let hitexp = hitSplosion (fst splitAliveDeadRes) secs

    -- Tabulate the score based on the dead entities
    let updatedScore = score gstate + calcScore (snd splitAliveDeadRes) 

    let entityFireBts = unzip (map (enemyFire updatedPlayer2 secs) hitexp)


    let resBg = updateBackgroundEntities gstate screenSize

    -- Update the status based on the health of the player
    let statusUpdate = if health (snd collisions) <= 0 then GameOver else status gstate

    return (gstate { status = statusUpdate
    , infoToShow = ShowANumber (round (health updatedPlayer2))
    , elapsedTime = elapsedTime gstate + secs
    , player = updatedPlayer2
    , enemies = fst entityFireBts ++ concat (snd entityFireBts) ++ newEnemies ++ explosions
    , background = resBg ++ newBg
    , timer = newTimeFreq
    , score = updatedScore })

-- | General update function for background entities
updateBackgroundEntities :: GameState -> (Int, Int) -> [Entity]
updateBackgroundEntities gstate screenSize = movedBg
  where
    remOffScreen = entityOffscreen (background gstate) screenSize
    movedBg = map moveEntity remOffScreen


-- | Check if the player or enemies get hit by bullets and damage them if necessary
collissionCheck :: [Entity] -> Player -> ([Entity], Player)
collissionCheck [] p = ([], p)
collissionCheck enms@(e:es) p = (damageEnt2, damagePlayer {bullets = damagePlayerBullets})
    where
      damagePlayer = collisionDamage enms p -- damage the player takes from colliding with entities
      damagePlayerBullets = map (collisionDamage enms) (bullets p) -- damage the bullets from the player
      damageEnt = map (collisionDamage (bullets p)) enms -- damage the entities take from bullets of the player
      damageEnt2 = map (collisionDamage [p]) damageEnt -- damage the entities take from the player from colliding

-- | Damage entities when they collide with another entity
collisionDamage :: [Entity] -> Entity -> Entity
collisionDamage [] e2 = e2
collisionDamage (e:es) e2
    | hitboxOverlap (hitbox e) (hitbox e2) = collisionDamage es (e2 {health = health e2 - damage e})
    | otherwise = collisionDamage es e2

-- | Check if two hitboxes overlap
hitboxOverlap :: HitBox -> HitBox -> Bool
hitboxOverlap ((Pt x1 y1), s1) ((Pt x2 y2), s2) =
  x1 < x2 + s2 &&
  y1 < y2 + s2 &&
  x2 < x1 + s1 &&
  y2 < y1 + s1

-- | Spawn explosions when rockets and worms die
necroSpawner :: [Entity] -> Float -> [Entity]
necroSpawner es secs= concatMap (\e -> [E Explosion (21 * secs) (fst (hitbox e), 20) None 5 (0, 0) (0, -1) [] | entityType e `elem` [Rocket, Brute]]) es

-- | Does damage to every explosion that exist to  
hitSplosion :: [Entity] -> Float -> [Entity]
hitSplosion es dmg = other ++ map (\e -> e {health = health e - dmg, hitbox = (fst (hitbox e), 4 * health e)}) explosions
    where
      explosions = filter (\e -> entityType e == Explosion) es
      other = filter (\e -> entityType e /= Explosion) es

-- | Split a list of entities into a list of alive ones and dead ones
splitAliveDead :: [Entity] -> ([Entity], [Entity])
splitAliveDead es = (alive, dead)
    where
      dead = filter (\e -> health e <= 0) es
      alive = filter (\e -> health e > 0) es



-- | Move the player according to their input
movementHandler :: [Char] -> (Int, Int) -> Player -> Player
movementHandler [] s p = p
movementHandler (c:chars) s@(xScreen, yScreen) p = movementHandler chars s newPlayer
  where
    (Pt x y) = fst (hitbox p)
    newPlayer = case c of
      'w' -> p {hitbox = (Pt x (min (y + 8) (fromIntegral (yScreen `div` 2))), snd (hitbox p))}
      'a' -> p {hitbox = (Pt (max (x - 8) (-fromIntegral (xScreen `div` 2))) y, snd (hitbox p))}
      's' -> p {hitbox = (Pt x (max (y - 8) (-fromIntegral (yScreen `div` 2))), snd (hitbox p))}
      'd' -> p {hitbox = (Pt (min (x + 8) (fromIntegral (xScreen `div` 2))) y, snd (hitbox p))}
      _ -> p -- if it gets a key it doesnt understand ignore it

-- | Move an entity
moveEntity :: Entity -> Entity
moveEntity e = e {hitbox = (newPt, s)}
    where
        (Pt x y, s)= hitbox e
        (dx, dy) = direction e
        newPt = if entityType e `elem` [Swarm, Brute] then Pt (x - dx) (y - f (x - dx)) else Pt (x - dx) (y - dy) -- use the function defined in the gamemechanics for the swarms


-- | Spawn entities based on their respective timers and frequencies
spawner :: [TimerFreq] -> Float -> (Int, Int) -> Pos -> [(TimerFreq, Maybe Entity)]
spawner timers secs (xScreen, yScreen) p = 
  map (\(T name time freq) -> 
    if time >= freq
      then (T name 0 freq, getEntity name) 
      else (T name (time + secs) freq, Nothing)) timers
    where
      getEntity :: EntityTypes -> Maybe Entity
      getEntity name = case name of
                Swarm -> Just (E Swarm 3 (p, swarmSize) Peashooter 1 (2, 0) (0, swarmRoF) [])
                Turret -> Just (E Turret 100000000 (Pt (fromIntegral (xScreen `div` 2)) (0 - fromIntegral (yScreen `div` 2)), turretSize) Peashooter 1 (2, 0) (0, turretRoF) [])
                Brute -> Just (E Brute 5 (p, bruteSize) Peashooter 1 (2, 0) (0, bruteRoF) [])
                --background entities
                Cloud -> Just (E Cloud 1 (p, cloudSize) None 0 (3, 0) (0, -1) [])
                Mountain -> Just (E Mountain 1 ((Pt (fromIntegral (xScreen `div` 2)+mountainSize) (- fromIntegral (yScreen `div` 2))), mountainSize) None 0 (1, 0) (0, -1) [])
                Planet -> Just (E Planet 1 (p, planetSize) None 0 (0.5, 0) (0, -1) [])
                _ -> Nothing


-- | Shoot new bullets based on the weapon and rate and update the timer
bulletHandler :: String -> Float -> Player -> ((Time, Freq), [Bullet])
bulletHandler keys secs p
    | charMember '.' keys && t >= f = ((0, f), [getBullet we (Pt x y)])
    | otherwise = ((t + secs, f), [])
  where
    (Pt x y) = fst (hitbox p)
    we = weapon p
    (t, f) = rate p
    
-- | Let the enemies fire bullets
enemyFire :: Player -> Float -> Entity -> (Entity, [Bullet])
enemyFire player secs e
  | t >= f = (e {rate = (0, f)}, b)
  | otherwise = (e {rate = (t + secs, f)}, [])
  where
    (t, f) = rate e
    ePos@(Pt xe ye) = fst (hitbox e)
     -- Let the enemies shoot toward the player
    (Pt xp yp) = fst (hitbox player)
    xdif = xp - xe
    ydif = yp - ye
    c = sqrt (xdif ^ 2 + ydif^2)
    d@(dx, dy) = (-xdif * 4 / c,-ydif * 4 / c)
    pea d = E Pea 1 (ePos, 10) None 1 d (0, -1) [] -- standard pea bullet format where only a direction needs to be added
    b = case entityType e of
        Swarm -> [pea d]
        Brute -> [pea (2, 2), pea (-2, 2), pea (-2, -2), pea (2, -2)]
        Turret -> [pea d, pea d]
        _ -> []

-- | Retrieve the bullet associated with a certain weapon
getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter p = E Pea 1 (p, 5) None 5 (-8, 0) (0, -1) []
getBullet Launcher p = E Rocket 1 (p, 10) None 5 (-8, 0) (0, -1) []
getBullet Laser p = E Laserbeam 1 (p, 10) None 5 (-8, 0) (0, -1) []   

-- | Change from a weapon to another weapon
switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter


-- | Calculate the score based on a list of dead enemies
calcScore :: [Enemy] -> Score
calcScore [] = 0
calcScore (e:es)
  | health e <= 0 = points + calcScore es
  | otherwise = calcScore es
  where
    points = case entityType e of
              Swarm -> 5
              Turret -> 10
              Brute -> 15
              _ -> 0


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate
  | isEsc = error "Game Exited"
  | status gstate == StartScreen = inputKeyStart e gstate
  | status gstate == Game = inputKeyGame e gstate
  | status gstate == Pause = inputKeyPause e gstate
  | status gstate == GameOver = inputKeyGameOver e gstate
  | otherwise = return gstate
  where
    (EventKey k s m p) = e
    isEsc = (EventKey (SpecialKey KeyEsc) s m p) == e

-- | Handle input at the start menu
inputKeyStart :: Event -> GameState -> IO GameState
inputKeyStart (EventKey (SpecialKey KeyEnter) Down _ _) gstate = return initialState {status = Game}
inputKeyStart (EventKey (Char '1') Down _ _) gstate =
  do
    files <- getDirectoryContents "saveFiles/"
    let firstFile = head files
    if length files > 2 
      then do 
        fileGstate <- readGameState ("saveFiles/" ++ firstFile)
        return (fromJust fileGstate)
      else return gstate
inputKeyStart e gstate = return gstate


-- | Handle movements which can be held during the game
inputKeyGame :: Event -> GameState -> IO GameState
inputKeyGame (EventKey (Char c) Down _ _) gstate | c == 'w' || c=='a' || c=='s' || c=='d' = return gstate {keys = insertChar c (keys gstate)}
inputKeyGame (EventKey (Char c) Up _ _) gstate = return gstate {keys = deleteChar c (keys gstate)}
inputKeyGame (EventKey (Char 'p') Down _ _) gstate = return gstate {keys="",status = Pause}
-- | Shoot bullets!
inputKeyGame (EventKey (SpecialKey KeySpace) Down _ _) gstate = return gstate { keys = insertChar '.' (keys gstate)}
inputKeyGame (EventKey (SpecialKey KeySpace) Up _ _) gstate = return gstate { keys = deleteChar '.' (keys gstate)}
-- | Switch weapon when possible
inputKeyGame (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = return gstate {player = (player gstate) {weapon = newWe, rate = (0, newF)} }
    where
      (E et health hb we dmg dir (t, f) b) = player gstate
      newWe = switchWeapon we
      newF = case newWe of
        Peashooter -> 0.5
        Launcher -> 1
        Laser -> 0.1
inputKeyGame e gstate = return gstate

-- | Handle input during a pause
inputKeyPause (EventKey (Char 'p') Down _ _) gstate = return gstate {status = Game}
inputKeyPause (EventKey (Char 'r') Down _ _) gstate = return initialState
inputKeyPause (EventKey (Char 's') Down _ _) gstate =
    if status gstate == Pause
    then do
      writeGameState "saveFiles/save1.json" (gstate {keys = ""}) -- currently only one saveslot
      putStrLn "SAVING YOUR GAME MAN"
      return gstate {infoToShow = ShowAString "Saving"}
    else return gstate
inputKeyPause e gstate = return gstate

-- | Handle keys at game over
inputKeyGameOver (EventKey (Char 'r') _ _ _) gstate = return initialState
inputKeyGameOver e gstate = return gstate

-- Helper functions for held input
insertChar :: Char -> String -> String
insertChar c s 
  | c `notElem` s = c : s
  | otherwise = s
deleteChar :: Char -> String -> String
deleteChar char s = filter (char /=) s
charMember :: Char -> String -> Bool
charMember c s = c `elem` s