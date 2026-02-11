{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Match
  ( Player (..),
    Schedule,
    DoublesTeam (..),
    Team (..),
    Matchup (..),
    makeSchedule,
    scheduleToList,
    teamToList,
  )
where

import Control.Monad.Trans.State.Lazy (State, state)
import Data.Aeson (FromJSON, ToJSON (toJSON), parseJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random.Stateful (RandomGen, randomR)

data Player = Player
  { name :: Text,
    rating :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Player

instance ToJSON Player

data Team = Team Player Player Player Player Player deriving (Eq, Show, Generic)

instance ToJSON Team where
  toJSON = toJSON . teamToList

instance FromJSON Team where
  parseJSON js =
    parseJSON js
      >>= ( \case
              [p1, p2, p3, p4, p5] -> pure $ Team p1 p2 p3 p4 p5
              players ->
                let numPlayers = length players
                 in if numPlayers < 5
                      then fail "Too few players"
                      else fail "Too many players"
          )

teamToList :: Team -> [Player]
teamToList (Team p1 p2 p3 p4 p5) = [p1, p2, p3, p4, p5]

data DoublesTeam = DoublesTeam Player Player deriving (Show, Generic)

instance ToJSON DoublesTeam
instance FromJSON DoublesTeam

data Matchup
  = SinglesMatchup (Player, Player)
  | DoublesMatchup (DoublesTeam, DoublesTeam)
  | TeamMatchup (Team, Team)
  deriving (Show, Generic)

instance ToJSON Matchup
instance FromJSON Matchup

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
instance FromJSON Day1

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
instance FromJSON Day2

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
instance FromJSON Day3

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
instance FromJSON Day4

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
instance FromJSON Schedule

shuffleTeam :: (RandomGen g) => Team -> State g Team
shuffleTeam team = state $ \gen ->
  go gen [] (teamToList team)
  where
    go g [p1', p2', p3', p4', p5'] _ = (Team p1' p2' p3' p4' p5', g)
    go g acc rest =
      -- randomR is a closed range, can't (!! length)
      let (idx, g') = randomR (0, length rest - 1) g
          selected = rest !! idx
       in go g' (acc <> [selected]) (filter (/= selected) rest)

singlesMatchup :: (RandomGen g) => Team -> Team -> State g Matchup
singlesMatchup teamA teamB = state $ \gen ->
  let (idx1, g') = randomR (0, 4) gen
      (idx2, g'') = randomR (0, 4) g'
   in (SinglesMatchup (teamToList teamA !! idx1, teamToList teamB !! idx2), g'')

-- |
--  In principle this doesn't have to go doubles / singles / doubles, but in the rules that's the order that
--  the constraint always shows up in 🤷
exhaustiveMatchSeq :: (RandomGen g) => Team -> Team -> State g (Matchup, Matchup, Matchup)
exhaustiveMatchSeq teamA teamB =
  do
    (Team a1' a2' a3' a4' a5') <- shuffleTeam teamA
    (Team b1' b2' b3' b4' b5') <- shuffleTeam teamB
    pure
      ( DoublesMatchup (DoublesTeam a1' a2', DoublesTeam b1' b2'),
        SinglesMatchup (a3', b3'),
        DoublesMatchup (DoublesTeam a4' a5', DoublesTeam b4' b5')
      )

makeDay1 :: (RandomGen g) => Team -> Team -> State g Day1
makeDay1 teamA teamB = do
  teamMatchOrderA <- shuffleTeam teamA
  teamMatchOrderB <- shuffleTeam teamB
  (doubles1, singles, doubles2) <- exhaustiveMatchSeq teamA teamB
  pure $ Day1 (TeamMatchup (teamMatchOrderA, teamMatchOrderB)) doubles1 singles doubles2

makeDay2 :: (RandomGen g) => Team -> Team -> State g Day2
makeDay2 teamA teamB = do
  shuffledA <- shuffleTeam teamA
  shuffledB <- shuffleTeam teamB
  let teamMatch = TeamMatchup (shuffledA, shuffledB)
  singles1 <- singlesMatchup teamA teamB
  (doubles1, singles2, doubles2) <- exhaustiveMatchSeq teamA teamB
  pure $ Day2 teamMatch singles1 doubles1 singles2 doubles2

makeDay3 :: (RandomGen g) => Team -> Team -> State g Day3
makeDay3 teamA teamB = do
  shuffledA <- shuffleTeam teamA
  shuffledB <- shuffleTeam teamB
  let teamMatch = TeamMatchup (shuffledA, shuffledB)
  singles1 <- singlesMatchup teamA teamB
  (doubles1, singles2, doubles2) <- exhaustiveMatchSeq teamA teamB
  singles3 <- singlesMatchup teamA teamB
  pure $ Day3 teamMatch singles1 doubles1 singles2 doubles2 singles3

makeDay4 :: (RandomGen g) => Team -> Team -> State g Day4
makeDay4 teamA teamB = do
  Team a1 a2 a3 a4 a5 <- shuffleTeam teamA
  Team b1 b2 b3 b4 b5 <- shuffleTeam teamB
  captainsPick <- singlesMatchup teamA teamB
  pure $
    Day4
      (SinglesMatchup (a1, b1))
      (SinglesMatchup (a2, b2))
      (SinglesMatchup (a3, b3))
      (SinglesMatchup (a4, b4))
      (SinglesMatchup (a5, b5))
      captainsPick

makeSchedule :: (RandomGen g) => Team -> Team -> State g Schedule
makeSchedule teamA teamB = do
  day1 <- makeDay1 teamA teamB
  day2 <- makeDay2 teamA teamB
  day3 <- makeDay3 teamA teamB
  day4 <- makeDay4 teamA teamB
  pure $ Schedule day1 day2 day3 day4

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
