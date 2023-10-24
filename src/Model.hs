-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char


data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   player :: Player,
                  --  enemies :: [Enemy], -- ik weet niet of dit de beste manier is om alle enemies te representeren, misschien aparte lijsten per enemy
                   enemies :: [Swarm],
                   score :: Score,
                   elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing (P (Pt 0 0) Peashooter 5 3 []) [Swarm 3 10 (Pt 0 0)] 0 0

data Player = P {
                position :: Point,
                weapon :: Weapon,
                speed :: Speed,
                health :: Health,
                bullets :: [Bullet]
                }

data Point = Pt Float Float
data Weapon = Peashooter | Launcher | Laser
type Bullet = Point

type Score = Int
type Health = Int
type Speed = Int
type Size = Float

data Swarm = Swarm Health Size Point

-- data Enemy = Swarm Health Size Point
--             | Turret Point -- does not have lives since you cant hit them
--             | Worm Point
--             | Boss Point
