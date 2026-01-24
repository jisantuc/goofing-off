{-# LANGUAGE DeriveGeneric #-}

module Match
  ( Day1 (..),
    Day2 (..),
    Day3 (..),
    Day4 (..),
    Player (..),
    Schedule(..),
    DoublesTeam (..),
    Team (..),
    Matchup (..),
    scheduleToList,
    teamToList,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Player = Player
  { name :: Text,
    rating :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Player

instance ToJSON Player

data Team = Team Player Player Player Player Player deriving (Eq, Show, Generic)

instance ToJSON Team

teamToList :: Team -> [Player]
teamToList (Team p1 p2 p3 p4 p5) = [p1, p2, p3, p4, p5]

data DoublesTeam = DoublesTeam Player Player deriving (Show, Generic)

instance ToJSON DoublesTeam

data Matchup
  = SinglesMatchup (Player, Player)
  | DoublesMatchup (DoublesTeam, DoublesTeam)
  | TeamMatchup (Team, Team)
  deriving (Show, Generic)

instance ToJSON Matchup

-- All players must play at least once in matches 2, 3, and 4.
data Day1
  = Day1
  { match1 :: Matchup,
    match2 :: Matchup,
    match3 :: Matchup,
    match4 :: Matchup
  }
  deriving (Show, Generic)

instance ToJSON Day1

-- All players must play at least once in matches 7, 8, and 9.
-- third/fourth/fifth match of the day
-- commentators also kept saying that the team matchup order had to be
-- different from day 1, so I'll include that constraint as well
data Day2
  = Day2
  { match5 :: Matchup,
    match6 :: Matchup,
    match7 :: Matchup,
    match8 :: Matchup,
    match9 :: Matchup
  }
  deriving (Generic, Show)

instance ToJSON Day2

-- All players must play at least once in matches 12, 13, and 14.
-- that's the third/fourth/fifth match of the day
data Day3
  = Day3
  { match10 :: Matchup,
    match11 :: Matchup,
    match12 :: Matchup,
    match13 :: Matchup,
    match14 :: Matchup,
    match15 :: Matchup
  }
  deriving (Generic, Show)

instance ToJSON Day3

-- |
-- "Each player must play one singles match before playing again."
-- I think that means you can't use a player a second time before each
-- player has played once, but really, that's just a guess.
data Day4
  = Day4
  { match16 :: Matchup,
    match17 :: Matchup,
    match18 :: Matchup,
    match19 :: Matchup,
    match20 :: Matchup,
    match21 :: Matchup
  }
  deriving (Generic, Show)

instance ToJSON Day4

-- |
-- - One issue is, this isn't really foldable, so keeping track of
-- - what match I'm in and whether to continue isn't easy.
-- - Something more obviously foldable (e.g. ADT for matchups) doesn't provide
-- - the same type safety around the rules.
-- - Maybe not exposing Schedule constructor would help with the unsafety?
data Schedule
  = Schedule
      Day1
      Day2
      Day3
      Day4
  deriving (Generic)

instance ToJSON Schedule

scheduleToList :: Schedule -> [Matchup]
scheduleToList
  ( Schedule
      (Day1 m1 m2 m3 m4)
      (Day2 m5 m6 m7 m8 m9)
      (Day3 m10 m11 m12 m13 m14 m15)
      (Day4 m16 m17 m18 m19 m20 m21)
    ) =
    [ m1,
      m2,
      m3,
      m4,
      m5,
      m6,
      m7,
      m8,
      m9,
      m10,
      m11,
      m12,
      m13,
      m14,
      m15,
      m16,
      m17,
      m18,
      m19,
      m20,
      m21
    ]
