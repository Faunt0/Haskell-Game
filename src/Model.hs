-- | This module contains the data types
--   which represent the state of the game
module Model where
-- import qualified Data.Set as S (Set, insert, delete, empty)
import qualified Data.Set as S hiding (map, filter)

-- swarmFreq :: IO Int
-- swarmFreq = return 1

peashooterRate :: Float
peashooterRate = 10
peashooterSpeed :: Float
peashooterSpeed = 20
launcherRate :: Float
launcherRate = 2
rocketSpeed :: Float
rocketSpeed = 40

-- laser implementeren word echt lastig
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
timersFreqs = [T "Swarm" 0 1, T "Worm" 0 10]

data Player = P {
                position :: Pos,
                weapon :: Weapon,
                speed :: Speed, -- weet niet hoe nuttig het is om dit te doen, is het handiger om het globaal te definieren
                health :: Health,
                playerTimer :: (Time, Freq),
                bullets :: [Bullet] -- change to also accept rockets etc
                }

data Weapon = Peashooter | Launcher | Laser 

instance Show Weapon where -- is dit nodig? ja voor het bepalen wat voor ammo er geschoten word toch
    show Peashooter = "PeaShooter"
    show Launcher = "Launcher"
    show Laser = "Laser"

data Bullet = Pea Pos 
      | Rocket Pos 
      | Laserbeam Pos

instance Show Bullet where
    show (Pea _) = "Pea"
    show (Rocket _) = "Rocket"
    show (Laserbeam _) = "Laserbeam"

type Score = Int
type Health = Int
type Speed = Int
type Size = Float


data Pos = Pt Float Float deriving (Eq)

-- hebben enemies ook een speed?
data Enemy = Swarm Health Pos Size
            | Turret Health Pos Size -- does not have lives since you cant hit them
            | Worm Health Pos Size
            | Boss Health Pos Size
            deriving (Eq)

-- misschien niet nodig
instance Show Enemy where
  show (Swarm {}) = "Swarm"
  show (Turret {}) = "Turret"
  show (Worm {}) = "Worm"
  show (Boss {}) = "Boss"