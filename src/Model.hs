-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow,
                   player :: Player,
                   elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing (P (Pt 0 0) Peashooter 5 3) 0

data Player = P Position Weapon Velocity Lives

data Weapon = Peashooter | Launcher | Laser

type Position = Point

data Point = Pt Float Float

type Velocity = Float

type Lives = Int

type Score = Int