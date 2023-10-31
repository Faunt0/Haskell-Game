-- | This module contains the data types
--   which represent the state of the game
module Model where
-- import qualified Data.Set as S (Set, insert, delete, empty)
import qualified Data.Set as S hiding (map, filter)

-- dit is een prima manier, dit is de b van y = ax + b wat handig is voor aanpassen als de score hoger wordt aangezien ik dan gewoon de formule kan intypen in de methode
peashooterRate :: Float
peashooterRate = 10
peashooterSpeed :: Float
peashooterSpeed = 20
launcherRate :: Float
launcherRate = 2
rocketSpeed :: Float
rocketSpeed = 40

-- laser implementeren word echt lastig
-- kan het door het een lijn te maken en de frequentie van de speler gewoon naar 0 te zetten?
laserRate :: Float
laserRate = 3 -- dit is anders
laserbeamSpeed :: Float
laserbeamSpeed = 10 -- is dit nodig als het een laser die aanstaat?



data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAString String

data TimerFreq = T String Time Freq
type Time = Float
type Freq = Float
type Damage = Int

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   keys :: S.Set Char,
                   timer :: [TimerFreq], -- a list (does this need to be a set? or not with correct implementation) of timers to spawn the enemies at certain rates
                   player :: Player,
                   enemies :: [Enemy],
                   score :: Score,
                   elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing S.empty timersFreqs (P (Pt 0 0) Peashooter 5 3 (0, 0) []) [] 0 0

timersFreqs :: [TimerFreq]
timersFreqs = [T "Swarm" 0 1, T "Worm" 0 5]

data Player = P {
                position :: Pos,
                weapon :: Weapon,
                speed :: Speed, -- weet niet hoe nuttig het is om dit te doen, is het handiger om het globaal te definieren
                health :: Health,
                playerTimer :: (Time, Freq),
                bullets :: [Bullet] -- change to also accept rockets etc
                }

data Weapon = Peashooter | Launcher | Laser

-- misschien ook representeren op de andere manier voor makkelijkere functie definitie en aanpassingen voor boosts etc
data Bullet = Bullet {
  bulletType :: BulletType,
  bulletPosition :: Pos,
  bulletDamage :: Damage,
  bulletSpeed :: Speed,
  bulletSize :: Size
} deriving Eq
data BulletType = Pea | Rocket | Laserbeam deriving Eq -- laserbeam might not be represented as a bullet

type Score = Int
type Health = Int
type Speed = Float
type Size = Float


data Pos = Pt Float Float deriving (Eq)

-- hebben enemies ook een speed? heb ik globaal gedefinieerd, hier alleen dingen die aangepast worden tijdens het spel zelf. misschien dat size niet nodig is dan?
-- hebben enemies ook hier gedefinieerd wat voor bullets ze afschieten? ja want dat verandert door de tijd
-- maak het globaal als ik wil dat het niet kan worden veranderd


-- misschien beter om een entity class te maken
-- kun je niet ook bullets modeleren als entities?
data Enemy = Enemy {
    enemyHealth :: Health
    , enemyPosition :: Pos
    , enemySize :: Size
    , enemyBullets :: [Bullet]
    , enemySpecies :: EnemySpecies
} deriving Eq
data EnemySpecies = Swarm | Worm | Turret | Boss deriving Eq