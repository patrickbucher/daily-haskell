module RockPaperScissors
  ( Player(First, Second)
  , Move(..)
  , Outcome(..)
  ) where

data Player
  = First
  | Second

data Move
  = Rock
  | Paper
  | Scissors

data Outcome
  = Win Player
  | Draw

data Athlete = Athlete
  { fullName :: String
  , age :: Int
  , country :: String
  , winRatio :: Float
  } deriving (Show)

joe :: Athlete
joe = Athlete "Joe Doe" 26 "USA" 0.37

jay :: Athlete
jay = Athlete {fullName = "Jay Day", winRatio = 0.68, age = 39, country = "GB"}

data Day
  = Mo
  | Tu
  | We
  | Th
  | Fr
  | Sa
  | Su
  deriving (Bounded, Enum, Show)

data WorkDay
  = Monday Bool
  | Tuesday Bool
  | Wednesday Bool
  | Thursday Bool
  | Friday Bool
  | Saturday Bool
  | Sunday Bool
  deriving (Bounded, Enum, Show)
