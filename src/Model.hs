-- | This module contains the data types
--   which represent the state of the game
module Model where

swarmFreq :: IO Int
swarmFreq = return 1



data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAString String


data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   player :: Player,
                   enemies :: [Enemy],
                   score :: Score,
                   elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing (P (Pt 0 0) Peashooter 5 3 []) [Swarm 3 (Pt 50 0) 20] 0 0

data Player = P {
                position :: Pos,
                weapon :: Weapon,
                speed :: Speed,
                health :: Health,
                bullets :: [Bullet] -- change to also accept rockets etc
                }

data Weapon = Peashooter | Launcher | Laser
-- type Bullet = Pos

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

-- data Swarm = Swarm Health Size Pos

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


-- do i have to work with monads??
-- data Ammo a = Ender Pos | Creeper a

-- instance Monad Ammo where
--   (Ender p) >>= f = Ender p 
--   return = Just