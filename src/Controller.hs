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
  | status gstate == StartScreen = return gstate {infoToShow = ShowAString "START MENU"}
  | status gstate == Pause = return gstate {infoToShow = ShowAString "PAUSE"}
  | status gstate == GameOver = return gstate {infoToShow = ShowAString "GAME OVER"}
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
    let movedPlayer = movementHandler (S.toList (keys gstate)) screenSize (player gstate) -- move the player within the boundaries


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



    let statusUpdate = if health (snd collisions) <= 0 then GameOver else status gstate -- update the status based on the health of the player

    return (gstate { status = statusUpdate
    , infoToShow = ShowANumber (round (health updatedPlayer2))
    , elapsedTime = elapsedTime gstate + secs
    , player = updatedPlayer2
    , enemies = fst entityFireBts ++ flatten (snd entityFireBts) ++ newEnemies ++ explosions
    , timer = newTimeFreq
    , score = updatedScore })


-- kan dit met een foldr
flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (h:t) = h ++ flatten t

-- laser dynamics?
bulletHandler :: Set Char -> Float -> Player -> ((Time, Freq), [Bullet])
bulletHandler set secs p
    | S.member '.' set && t >= f = ((0, f), [getBullet we (Pt x y)])
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

-- is this possible?
-- removeDead es = [E Explosion 5 (fst (hitbox e), 20) None 5 (0, const 0) (0, -1) [] | e <- es, entityType e `elem` [Rocket] && health e < 0] -- spawn explosions if certain entities die

-- removeDead [] = []
-- removeDead (e:es) -- = necro ++ removeDead es
--     -- | health e <= 0 && entityType e `elem` [Rocket] = necro : removeDead es
--     | health e <= 0 = necro ++ removeDead es
--     | otherwise = e : removeDead es
--     where
--       (Pt x y) = fst (hitbox e)
--       -- necro = [E Explosion 5 ((Pt x y), 20) None 5 (4, const 4) (0, -1) [] | entityType e `elem` [Rocket] && health e < 0]
--       -- necro = E Explosion 5 ((Pt x y), 20) None 5 (0, const 0) (0, -1) []
--       necro = if entityType e `elem` [Rocket] && health e <= 0
--         then [E Explosion 5 ((Pt x y), 20) None 5 (0, const 0) (0, -1) []]
--         else []



-- beetje randomizen
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
    d@(dx, dy) = (-xdif * 4 / c, -ydif * 4 / c)

    b = case entityType e of
        Swarm -> [E Pea 1 (ePos, 10) None 5 d (0, -1) []]
        Worm -> []
        Turret -> [E Pea 1 (ePos, 10) None 5 d (0, -1) [], E Pea 1 (ePos, 10) None 5 d (0, -1) []]
        Boss -> [E Rocket 1 (ePos, 10) None 5 d (0, -1) []]
        _ -> []


moveEntity :: Entity -> Entity
moveEntity e = e {hitbox = (newPt, s)}
    where
        ((Pt x y), s)= hitbox e
        (dx, dy) = direction e
        newPt = if entityType e `elem` [Swarm, Worm] then Pt (x - dx) (y - f (x - dx)) else Pt (x - dx) (y - dy) -- use the function defined in the gamemechanics for the swarms



-- | Split the list of enemies into a list of alive enemies and dead ones
splitEntities :: [Enemy] -> [Enemy] -> [Enemy] -> ([Enemy], [Enemy]) -- maybe define new types to distinguish better what each list represents
splitEntities [] alive dead = (alive, dead)
splitEntities (e:es) alive dead
    | health e <= 0 = splitEntities es alive (e:dead)
    | otherwise = splitEntities es (e:alive) dead


spawner :: [TimerFreq] -> Float -> Score -> (Int, Int) -> Pos -> [(TimerFreq, Maybe Enemy)]
-- spawner [] _ _ _ _ = []
-- spawner ((T name time freq):r) secs score screen@(xScreen, yScreen) p
--     | time >= freq = ((T name 0 freq), Just enemy) : spawner r secs score screen p
--     | otherwise = ((T name (time + secs) freq), Nothing) : spawner r secs score screen p
spawner timers secs score (xScreen, yScreen) p = map (\(T name time freq) -> if time >= freq then (T name 0 (alterFreq freq score), Just (getEntity name)) else (T name (time + secs) freq, Nothing)) timers
    where
      alterFreq :: Freq -> Score -> Freq
      alterFreq freq s = freq
      -- alterFreq freq s = freq / (freq + fromIntegral s) -- base the spawnrates on the score


      getEntity :: String -> Entity
      getEntity name
          | score > 500 = E Boss 5 ((Pt 600 100), wormSize) Peashooter 50 (0, 0) (0, wormRoF) [] -- misschien niet hier, maak aparte boss functie met aparte spawners enzo en waves gebaseerd op zn health. moet er maar eentje spawnen en dan niet andere enemies
          | otherwise = case name of
                  -- "Swarm" -> E Swarm 3 (p, swarmSize) Peashooter 5 (0, 0) (0, swarmRoF - (fromIntegral score/20)) [] -- als je dit doet krijg je dat ze ineens enorm vaak schieten, dan lijkt de hitbox niet meer te werken?
                  "Swarm" -> E Swarm 3 (p, swarmSize) Peashooter 5 (2, 0) (0, swarmRoF) []
                  "Turret" -> E Turret 100000000 ((Pt (fromIntegral (xScreen `div` 2)) (0 - fromIntegral (yScreen `div` 2))), turretSize) Peashooter 10 (2, 0) (0, turretRoF) []
                  "Worm" -> E Worm 5 (p, wormSize) Peashooter 50 (2, 0) (0, wormRoF) []
        -- baseer het spawnen van de boss op de score, misschien een if then else gebruiken om alleen een boss te spawnen als de score zo hoog is en anders gewone enemies te spawnen.

-- bossFight :: [] -> Score
-- baseer het aantal enemies wat spawnt op de score en het hp van de boss

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
      _ -> p -- if it gets a key it doesnt understand ignore it (redundant if well implemented)



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
getBullet Laser p = E Laserbeam 1 (p, 10) None 5 (-8, 0) (0, -1) []       -- alter the way this works, meaning also having to alter bulletmovements


switchWeapon :: Weapon -> Weapon
switchWeapon Peashooter = Launcher
switchWeapon Launcher = Laser
switchWeapon Laser = Peashooter


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = inputKey e gstate

-- | Handle movements which can be held
inputKey :: Event -> GameState -> IO GameState
inputKey (EventKey (Char c) Down _ _) gstate
  | c == 'w' || c=='a' || c=='s' || c=='d' = return gstate {keys = S.insert c (keys gstate)}
  -- | otherwise = gstate
inputKey (EventKey (Char c) Up _ _) gstate = return gstate {keys = S.delete c (keys gstate)}

-- | Shoot bullets!
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = return gstate { keys = S.insert '.' (keys gstate)}
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = return gstate { keys = S.delete '.' (keys gstate)}

-- | Switch weapon when possible
inputKey (EventKey (SpecialKey KeyTab) Down _ _) gstate
  = return gstate { infoToShow = ShowAString "SW", player = (player gstate) {weapon = newWe, rate = (0, newF)} }
    where
      (E et health hb we dmg dir (t, f) b) = player gstate
      newWe = switchWeapon we
      newF = case newWe of
        Peashooter -> 0.5
        Launcher -> 1
        Laser -> 0.1

inputKey (EventKey (Char 'p') Down _ _) gstate = if status gstate == Pause then return gstate {status = Game} else return gstate {status = Pause}
inputKey (EventKey (Char 'r') _ _ _) gstate = return initialState -- restart, this should only happen when the status is gameover, Maybe also a clickable button like the save
-- als de muis binnen het vierkant van de "save" knop geklikt word
-- inputKey (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate = 
--     if status gstate == Pause && hitboxOverlap (Pt x y, 1) (Pt (-200) (-100), 50)
inputKey (EventKey (Char 'l') Down _ _) gstate = 
    if status gstate == Pause
    then do 
      writeGameState "saveFiles/save1.json" gstate
      putStrLn "SAVING YOUR GAME MAN"
      return gstate {infoToShow = ShowAString "SAVING"}
    else return gstate
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate = error "Game exited"
inputKey _ gstate = return gstate -- Otherwise keep the same