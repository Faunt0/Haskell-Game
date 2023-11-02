-- | This module contains the data types
--   which represent the state of the game
module Model where
-- import qualified Data.Set as S (Set, insert, delete, empty)
import qualified Data.Set as S hiding (map, filter)

import GameMechanics


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAString String

data TimerFreq = T String Time Freq
type Time = Float
type Freq = Float
type Damage = Int

type Score = Int
type Health = Int
type Formula = Float -> Float -- this can be useful for making the directions more complex
type Direction = (Float, Formula) 
type Size = Float
data Pos = Pt Float Float deriving (Eq)
type HitBox = (Pos, Size)

type Player = Entity
type Enemy = Entity
type Bullet = Entity
data Entity = E {
      entityType :: EntityTypes,
      health :: Health,
      hitbox :: HitBox,
      weapon :: Weapon,
      damage :: Damage, -- on collision with another entity
      direction :: Direction,
      rate :: (Time, Freq),
      bullets :: [Bullet] -- dit is niet nodig, als ik dit weg haal moeten we nadenken over of je kogels uit de lucht wil kunnen schieten
} --deriving Eq

data EntityTypes = Player | Worm | Swarm | Turret | Boss | Pea | Rocket | Laserbeam | Grenade | Explosion deriving Eq
data Weapon = None | Peashooter | Launcher | Laser deriving Eq
data Status = StartScreen | Game | Pause | GameOver deriving Eq

-- moet misschien nog buttons eraan toevoegen
data GameState = GameState {
                   status :: Status,
                   infoToShow  :: InfoToShow,
                   keys :: S.Set Char,
                   timer :: [TimerFreq], -- moet ik dit wel meegeven, kan ik dit niet beter gewoon aflezen? a list of timers to spawn the enemies at certain rates
                   player :: Entity,
                   enemies :: [Entity],
                   score :: Score,
                   elapsedTime :: Float -- heb het eigenlijk niet nodig
                 }

-- let op dat je hier dingen globaal definieert
initialState :: GameState
initialState = GameState Game ShowNothing S.empty spawnRate initialPlayer [] 0 0
initialPlayer :: Entity
initialPlayer = E Player 100 ((Pt 0 0), 10) Peashooter 50 (0, const 0) (0, 0.5) []


spawnRate :: [TimerFreq] -- kan niet in de enemy zelf omdat die niet nieuwe enimies kan spawnen
spawnRate = [T "Swarm" 0 1, T "Worm" 0 5, T "Turret" 0 6] -- spawnrates of the different enemies, this can be adjusted based on the score.


