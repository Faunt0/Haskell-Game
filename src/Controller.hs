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
import GameMechanics
import GHC.Base (undefined) -- unnecessary
import System.Directory (getDirectoryContents)


-- TODOS
-- achtergrond elementen
-- comments
-- pas gamemechanics aan
-- tweede gamedesign doc aanmaken
-- save file laden
-- startscherm maken -- bitmaps
-- turret bitmap zoeken?
-- code opschonen
-- tutorial schermpje

-- maybe make more packages to have a cleaner file
-- Misschien een sniper maken of burst in meerdere richtingen
-- boosts/health maken
-- artwork
-- background elements
-- werken met ammo en reload systemen zodat je niet gewoon je knoppen kunt blijven spammen?
-- iets met dat je een moederschip moet beschermen wat damage krijgt van collissions met enemies
-- enemies die stil staan op gegeven momenten en gewoon schieten
-- een soort backup players die achter de Player schieten.
-- dodge roll met i-frames


-- | Handle one iteration of the game
-- | Move background on the screen
-- | Enemies spawn, move here and attack here
-- | Spawn enemies here based on a frequency which is correlated to the score of the player
step :: Float -> GameState -> IO GameState
step secs gstate
  | status gstate == StartScreen = return gstate {infoToShow = ShowAString "START MENU"} -- dit moet in de view gebeuren toch? nergens anders
  | status gstate == Pause = return gstate {infoToShow = ShowAString "PAUSE"}
  | status gstate == GameOver = return gstate --{infoToShow = ShowAString "GAME OVER"}
  | otherwise =
  do
    -- spawn new enemies
    screenSize <- getScreenSize
    let xScreen = fst screenSize
    let yScreen = snd screenSize
    let margin = 0

    random1 <- randomRIO (1, 5) :: IO Int
    random2 <- randomRIO (- fromIntegral yScreen + margin, fromIntegral yScreen - margin) :: IO Float
    let randomY = random2


    -- Spawner
    let res = unzip (spawner (timer gstate) secs (score gstate) screenSize (Pt (fromIntegral (xScreen `div` 2) - margin) randomY))
    let newTimeFreq = fst res
    let newEnemies = catMaybes (snd res)

    -- check if any bullet hits an enemy and moves the bullet
    -- make a central update player and central update entities function for as far as it goes
    let movedPlayer = movementHandler (keys gstate) screenSize (player gstate) -- move the player within the boundaries


    let movedBullets = map moveEntity (bullets movedPlayer)  -- move the bullets of the player forward
    let removeOffscreenBullets = entityOffscreen movedBullets screenSize -- remove the bullets that are offscreen
    let bulletHandlerRes = bulletHandler (keys gstate) secs movedPlayer -- add new bullets based on the weapon and rate (and amount maybe) and update the timer
    let updatedPlayer = movedPlayer {bullets = removeOffscreenBullets ++ snd bulletHandlerRes, rate = fst bulletHandlerRes} -- update them for the player


    -- Handle the entities except the player
    let movedEntities = map moveEntity (enemies gstate) -- move all the other entities except the player.
    let onScreenEntities = entityOffscreen movedEntities screenSize

    let collisions = collissionCheck onScreenEntities updatedPlayer -- check if the enemies get shot and do the same for the player

    let splitEntitiesRes = splitEntities (fst collisions) [] [] -- split the current enemies into the alive and dead ones -- maybe use this list to see if rockets are dead and need to spawn another explosion? or you could make an enemy which upon dying spawns a couple of other smaller enemies

    let somenoame = splitEntities (bullets (snd collisions)) [] []
    let updatedPlayer2 = (snd collisions) {bullets = fst somenoame} -- only keep the alive bullets
    let explosions = necroSpawner (snd somenoame) secs -- moet ook spawnen voor sommige enemies niet alleen voor de dode rockets van de speler
    let hitexp = hitExplosions2 (fst splitEntitiesRes) secs -- hit all the explosions with certain damage
    let updatedScore = score gstate + calcScore (snd splitEntitiesRes) -- tabulate the score based on the alive entities

    let entityFireBts = unzip (map (enemyFire updatedPlayer2 secs) hitexp) -- let the alive entities fire bullets and add them to the entities list

    let noOffscreenBg = entityOffscreen (background gstate) screenSize
    let movedBg = map moveEntity noOffscreenBg
    let bgSpawnRes = unzip (backgroundSpawner newTimeFreq secs screenSize (Pt (fromIntegral (xScreen `div` 2) - margin) randomY))
    let newTimers = fst bgSpawnRes
    let newBg = catMaybes (snd bgSpawnRes)


    let statusUpdate = if health (snd collisions) <= 0 then GameOver else status gstate -- update the status based on the health of the player

    return (gstate { status = statusUpdate
    , infoToShow = ShowANumber (round (health updatedPlayer2))
    , elapsedTime = elapsedTime gstate + secs
    , player = updatedPlayer2
    , enemies = fst entityFireBts ++ flatten (snd entityFireBts) ++ newEnemies ++ explosions
    , background = movedBg ++ newBg
    , timer = newTimers
    , score = updatedScore })


-- eigelijk hetzelfde als concat, vervang dit
flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (h:t) = h ++ flatten t

-- laser dynamics?
bulletHandler :: String -> Float -> Player -> ((Time, Freq), [Bullet])
bulletHandler keys secs p
    | charMember '.' keys && t >= f = ((0, f), [getBullet we (Pt x y)])
    | otherwise = ((t + secs, f), [])
  where
    (Pt x y) = fst (hitbox p)
    we = weapon p
    (t, f) = rate p


collissionCheck :: [Entity] -> Player -> ([Entity], Player)
collissionCheck [] p = ([], p)
collissionCheck enms@(e:es) p = (damageEnt2, damagePlayer {bullets = damagePlayerBullets})
    where
      damagePlayer = collisionDamage enms p -- damage the player takes from colliding with entities
      damagePlayerBullets = map (collisionDamage enms) (bullets p) -- damage the bullets from the player
      damageEnt = map (collisionDamage (bullets p)) enms -- damage the entities take from bullets of the player
      damageEnt2 = map (collisionDamage [p]) damageEnt -- damage the entities take from the player from colliding

collisionDamage :: [Entity] -> Entity -> Entity
collisionDamage [] e2 = e2
collisionDamage (e:es) e2
    | hitboxOverlap (hitbox e) (hitbox e2) = collisionDamage es (e2 {health = health e2 - damage e})
    | otherwise = collisionDamage es e2

necroSpawner :: [Entity] -> Float -> [Entity]
necroSpawner es secs= flatten (map (\e -> [E Explosion (21 * secs) (fst (hitbox e), 20) None 5 (0, 0) (0, -1) [] | entityType e == Rocket]) es)

hitExplosions2 :: [Entity] -> Float -> [Entity]
hitExplosions2 es dmg = other ++ map (\e -> e {health = health e - dmg, hitbox = (fst (hitbox e), 4 * health e)}) explosions
    where
      explosions = filter (\e -> entityType e == Explosion) es
      other = filter (\e -> entityType e /= Explosion) es


-- moet ik hier nog iets doen over dat een dode raket een explosie kan veroorzaken? of een kamikaze een explosie veroorzaakt
removeDead :: [Entity] -> [Entity]
removeDead es = filter (\e -> health e > 0) es


-- bit more intuitive than the other splitEntities function
splitAliveDead :: [Entity] -> ([Entity], [Entity])
splitAliveDead es = (alive, dead)
    where
      dead = filter (\e -> health e <= 0) es
      alive = filter (\e -> health e > 0) es


-- | the enemies fire their bullets
enemyFire :: Player -> Float -> Entity -> (Entity, [Bullet])
enemyFire player secs e
  | t >= f = (e {rate = (0, f)}, b)
  | otherwise = (e {rate = (t + secs, f)}, [])
  where
    (t, f) = rate e
    ePos@(Pt xe ye) = fst (hitbox e)
    (Pt xp yp) = fst (hitbox player)
    xdif = xp - xe
    ydif = yp - ye
    c = sqrt (xdif ^ 2 + ydif^2)
    d@(dx, dy) = (-xdif * 4 / c,-ydif * 4 / c)

    b = case entityType e of
        Swarm -> [E Pea 1 (ePos, 10) None 1 d (0, -1) []]
        Worm -> []
        Turret -> [E Pea 1 (ePos, 10) None 1 d (0, -1) [], E Pea 1 (ePos, 10) None 5 d (0, -1) []]
        Boss -> [E Rocket 1 (ePos, 10) None 1 d (0, -1) []]
        _ -> []


moveEntity :: Entity -> Entity
moveEntity e = e {hitbox = (newPt, s)}
    where
        (Pt x y, s)= hitbox e
        (dx, dy) = direction e
        newPt = if entityType e `elem` [Swarm, Worm] then Pt (x - dx) (y - f (x - dx)) else Pt (x - dx) (y - dy) -- use the function defined in the gamemechanics for the swarms



-- | Split the list of enemies into a list of alive enemies and dead ones
splitEntities :: [Enemy] -> [Enemy] -> [Enemy] -> ([Enemy], [Enemy]) -- maybe define new types to distinguish better what each list represents
splitEntities [] alive dead = (alive, dead)
splitEntities (e:es) alive dead
    | health e <= 0 = splitEntities es alive (e:dead)
    | otherwise = splitEntities es (e:alive) dead


spawner :: [TimerFreq] -> Float -> Score -> (Int, Int) -> Pos -> [(TimerFreq, Maybe Enemy)]
spawner timers secs score (xScreen, yScreen) p = 
  map (\(T name time freq) -> 
    if time >= freq 
      then (T name 0 (alterFreq freq score), getEntity name) 
      else (T name (time + secs) freq, Nothing)) timers
    where
      alterFreq :: Freq -> Score -> Freq
      alterFreq freq s = freq
      -- alterFreq freq s = freq / (freq + fromIntegral s) -- base the spawnrates on the score
      getEntity :: EntityTypes -> Maybe Entity
      getEntity name
          | score > 500 = Just (E Boss 5 (Pt 600 100, wormSize) Peashooter 50 (0, 0) (0, wormRoF) []) -- misschien niet hier, maak aparte boss functie met aparte spawners enzo en waves gebaseerd op zn health. moet er maar eentje spawnen en dan niet andere enemies
          | otherwise = case name of
                  -- "Swarm" -> E Swarm 3 (p, swarmSize) Peashooter 5 (0, 0) (0, swarmRoF - (fromIntegral score/20)) [] -- als je dit doet krijg je dat ze ineens enorm vaak schieten, dan lijkt de hitbox niet meer te werken?
                  Swarm -> Just (E Swarm 3 (p, swarmSize) Peashooter 1 (2, 0) (0, swarmRoF) [])
                  Turret -> Just (E Turret 100000000 (Pt (fromIntegral (xScreen `div` 2)) (0 - fromIntegral (yScreen `div` 2)), turretSize) Peashooter 1 (2, 0) (0, turretRoF) [])
                  Worm -> Just (E Worm 5 (p, wormSize) Peashooter 1 (2, 0) (0, wormRoF) [])
                  _ -> Nothing
        -- baseer het spawnen van de boss op de score, misschien een if then else gebruiken om alleen een boss te spawnen als de score zo hoog is en anders gewone enemies te spawnen.
-- bossFight :: [] -> Score
-- baseer het aantal enemies wat spawnt op de score en het hp van de boss


backgroundSpawner :: [TimerFreq] -> Float -> (Int, Int) -> Pos -> [(TimerFreq, Maybe Entity)]
backgroundSpawner timers secs (xScreen, yScreen) p = 
  map (\(T name time freq) -> 
    if time >= freq 
      then (T name 0 freq, getEntity name) 
      else (T name (time + secs) freq, Nothing)) timers
    where
      getEntity :: EntityTypes -> Maybe Entity
      getEntity name = case name of
        Cloud -> Just (E Cloud 1 (p, cloudSize) None 0 (3, 0) (0, -1) [])
        Mountain -> Just (E Mountain 1 (p, mountainSize) None 0 (3, 0) (0, -1) [])
        Planet -> Just (E Planet 1 (p, planetSize) None 0 (3, 0) (0, -1) [])
        _ -> Nothing


calcScore :: [Enemy] -> Score
calcScore [] = 0
calcScore (e:es)
  | health e <= 0 = points + calcScore es
  | otherwise = calcScore es
  where
    points = case entityType e of
              Swarm -> 5 -- how should I define this properly? things like globally defined variables in packages
              Turret -> 10
              Worm -> 15
              Boss -> 50
              _ -> 0

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



-- misschien size een (Float, Float) maken zodat je ook rechthoeken/balken kunt hebben
hitboxOverlap :: HitBox -> HitBox -> Bool
hitboxOverlap ((Pt x1 y1), s1) ((Pt x2 y2), s2) =
  x1 < x2 + s2 &&
  y1 < y2 + s2 &&
  x2 < x1 + s1 &&
  y2 < y1 + s1


getBullet :: Weapon -> Pos -> Bullet
getBullet Peashooter p = E Pea 1 (p, 5) None 5 (-8, 0) (0, -1) []
getBullet Launcher p = E Rocket 1 (p, 10) None 5 (-8, 0) (0, -1) []
getBullet Laser p = E Laserbeam 1 (p, 10) None 5 (-8, 0) (0, -1) []   


switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter


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

inputKeyStart :: Event -> GameState -> IO GameState
inputKeyStart (EventKey (SpecialKey KeyEnter) Down _ _) gstate = return initialState {status = Game}
inputKeyStart (EventKey (Char '1') Down _ _) gstate =
  do
    files <- getDirectoryContents "saveFiles/"
    let firstFile = head files
    fileGstate <- readGameState ("saveFiles/" ++ firstFile)
    let g = fromJust fileGstate
    return g
inputKeyStart e gstate = return gstate


-- | Handle movements which can be held
inputKeyGame :: Event -> GameState -> IO GameState
inputKeyGame (EventKey (Char c) Down _ _) gstate | c == 'w' || c=='a' || c=='s' || c=='d' = return gstate {keys = insertChar c (keys gstate)}
inputKeyGame (EventKey (Char c) Up _ _) gstate = return gstate {keys = deleteChar c (keys gstate)}
inputKeyGame (EventKey (Char 'p') Down _ _) gstate = return gstate {status = Pause}
-- | Shoot bullets!
inputKeyGame (EventKey (SpecialKey KeySpace) Down _ _) gstate = return gstate { keys = insertChar '.' (keys gstate)}
inputKeyGame (EventKey (SpecialKey KeySpace) Up _ _) gstate = return gstate { keys = deleteChar '.' (keys gstate)}
-- | Switch weapon when possible
inputKeyGame (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = return gstate { infoToShow = ShowAString "SW", player = (player gstate) {weapon = newWe, rate = (0, newF)} }
    where
      (E et health hb we dmg dir (t, f) b) = player gstate
      newWe = switchWeapon we
      newF = case newWe of
        Peashooter -> 0.5
        Launcher -> 1
        Laser -> 0.1
inputKeyGame e gstate = return gstate


inputKeyPause (EventKey (Char 'p') Down _ _) gstate = return gstate {status = Game}
-- als de muis binnen het vierkant van de "save" knop geklikt word
-- inputKey (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate = 
--     if status gstate == Pause && hitboxOverlap (Pt x y, 1) (Pt (-200) (-100), 50)
inputKeyPause (EventKey (Char 's') Down _ _) gstate =
    if status gstate == Pause
    then do
      writeGameState "saveFiles/save1.json" (gstate {keys = ""})
      putStrLn "SAVING YOUR GAME MAN"
      return gstate {infoToShow = ShowAString "SAVING"}
    else return gstate
inputKeyPause e gstate = return gstate

inputKeyGameOver (EventKey (Char 'r') _ _ _) gstate = return initialState
inputKeyGameOver e gstate = return gstate

-- to replace using set
insertChar :: Char -> String -> String
insertChar c s 
  | c `notElem` s = c : s
  | otherwise = s
deleteChar :: Char -> String -> String
deleteChar char s = filter (char /=) s
charMember :: Char -> String -> Bool
charMember c s = c `elem` s