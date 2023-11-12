-- | This module contains the data types
--   which represent the state of the game
{-# LANGUAGE DeriveGeneric #-}


module Model where
-- import qualified Data.Set as S (Set, insert, delete, empty)
import qualified Data.Set as S hiding (map, filter)
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import GHC.Generics
import Data.Aeson
import GameMechanics

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
                 } deriving (Generic, Show)

-- let op dat je hier dingen globaal definieert
initialState :: GameState
initialState = GameState Game ShowNothing S.empty spawnRate initialPlayer [] 0 0
initialPlayer :: Entity
--initialPlayer = E Player 100 ((Pt 0 0), 10) Peashooter 50 (0, 0) (0, 0.5) []
initialPlayer = E Player 5 (Pt 0 0, 10) Peashooter 50 (0, 0) (0, 0.5) []
spawnRate :: [TimerFreq] -- kan niet in de enemy zelf omdat die niet nieuwe enimies kan spawnen
spawnRate = [T "Swarm" 0 0.5, T "Worm" 0 5, T "Turret" 0 3] -- spawnrates of the different enemies, this can be adjusted based on the score.



data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAString String deriving (Generic, Show)

data TimerFreq = T String Time Freq deriving (Generic, Show, Eq)
type Time = Float
type Freq = Float
type Damage = Float

type Score = Int
type Health = Float
-- type Formula = Float -> Float  -- this can be useful for making the directions more complex
-- type Direction = (Float, Formula)
type Direction = (Float, Float) 
type Size = Float
data Pos = Pt Float Float deriving (Generic, Show, Eq)
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
} deriving (Generic, Show)

data EntityTypes = Player | Worm | Swarm | Turret | Boss | Pea | Rocket | Laserbeam | Grenade | Explosion deriving (Generic, Show, Eq)
data Weapon = None | Peashooter | Launcher | Laser deriving (Generic, Show)
data Status = StartScreen | Game | Pause | GameOver deriving (Generic, Show, Eq)


instance ToJSON GameState
instance ToJSON Entity
instance ToJSON Status
instance ToJSON Weapon
instance ToJSON EntityTypes
instance ToJSON Pos
instance ToJSON InfoToShow
instance ToJSON TimerFreq
instance FromJSON GameState
instance FromJSON Entity
instance FromJSON Status
instance FromJSON Weapon
instance FromJSON EntityTypes
instance FromJSON Pos
instance FromJSON InfoToShow
instance FromJSON TimerFreq


writeGameState :: FilePath -> GameState -> IO () -- haal de filenamen
writeGameState filePath gameState = do
  let json = encode gameState
  B.writeFile filePath json
readGameState :: FilePath -> IO (Maybe GameState)
readGameState filePath = do
  fileContents <- B.readFile filePath
  return (decode fileContents)