{-# LANGUAGE DataKinds #-}

module Match
  ( Schedule,
    Team (..),
    makeSchedule,
  )
where

import Data.Text (Text)
import System.Random.Stateful (RandomGen, randomR)

data Player = Player
  { name :: Text,
    rating :: Int
  }
  deriving (Eq, Show)

data Team = Team Player Player Player Player Player deriving (Eq, Show)

toList :: Team -> [Player]
toList (Team p1 p2 p3 p4 p5) = [p1, p2, p3, p4, p5]

data DoublesTeam = DoublesTeam Player Player deriving (Show)

newtype SinglesMatchup = SinglesMatchup (Player, Player) deriving (Show)

newtype DoublesMatchup = DoublesMatchup (DoublesTeam, DoublesTeam) deriving (Show)

newtype TeamMatchup = TeamMatchup (Team, Team) deriving (Eq, Show)

-- All players must play at least once in matches 2, 3, and 4.
data Day1
  = Day1
      TeamMatchup
      DoublesMatchup
      SinglesMatchup
      DoublesMatchup
  deriving (Show)

-- All players must play at least once in matches 7, 8, and 9.
-- third/fourth/fifth match of the day
-- commentators also kept saying that the team matchup order had to be
-- different from day 1, so I'll include that constraint as well
data Day2
  = Day2
      TeamMatchup
      SinglesMatchup
      DoublesMatchup
      SinglesMatchup
      DoublesMatchup
  deriving (Show)

-- All players must play at least once in matches 12, 13, and 14.
-- that's the third/fourth/fifth match of the day
data Day3
  = Day3
      TeamMatchup
      SinglesMatchup
      DoublesMatchup
      SinglesMatchup
      DoublesMatchup
      SinglesMatchup
  deriving (Show)

-- |
-- "Each player must play one singles match before playing again."
-- I think that means you can't use a player a second time before each
-- player has played once, but really, that's just a guess.
data Day4
  = Day4
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
  deriving (Show)

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

shuffleTeam :: (RandomGen g) => g -> Team -> (Team, g)
shuffleTeam gen team =
  go gen [] (toList team)
  where
    go g [p1', p2', p3', p4', p5'] _ = (Team p1' p2' p3' p4' p5', g)
    go g acc rest =
      -- randomR is a closed range, can't (!! length)
      let (idx, g') = randomR (0, length rest - 1) g
          selected = rest !! idx
       in go g' (acc <> [selected]) (filter (/= selected) rest)

singlesMatchup :: (RandomGen g) => g -> Team -> Team -> (SinglesMatchup, g)
singlesMatchup gen teamA teamB =
  let (idx1, g') = randomR (0, 4) gen
      (idx2, g'') = randomR (0, 4) g'
   in (SinglesMatchup (toList teamA !! idx1, toList teamB !! idx2), g'')

-- |
--  In principle this doesn't have to go doubles / singles / doubles, but in the rules that's the order that
--  the constraint always shows up in 🤷
exhaustiveMatchSeq :: (RandomGen g) => g -> Team -> Team -> ((DoublesMatchup, SinglesMatchup, DoublesMatchup), g)
exhaustiveMatchSeq gen teamA teamB =
  let (Team a1' a2' a3' a4' a5', g') = shuffleTeam gen teamA
      (Team b1' b2' b3' b4' b5', g'') = shuffleTeam g' teamB
   in ( ( DoublesMatchup (DoublesTeam a1' a2', DoublesTeam b1' b2'),
          SinglesMatchup (a3', b3'),
          DoublesMatchup ((DoublesTeam a4' a5'), (DoublesTeam b4' b5'))
        ),
        g''
      )

makeDay1 :: (RandomGen g) => g -> Team -> Team -> (Day1, g)
makeDay1 gen teamA teamB =
  let (teamMatchOrderA, g') = shuffleTeam gen teamA
      (teamMatchOrderB, g'') = shuffleTeam g' teamB
      ((doubles1, singles, doubles2), g''') = exhaustiveMatchSeq g'' teamA teamB
   in (Day1 (TeamMatchup (teamMatchOrderA, teamMatchOrderB)) doubles1 singles doubles2, g''')

makeDay2 :: (RandomGen g) => g -> Team -> Team -> (Day2, g)
makeDay2 gen teamA teamB =
  let (shuffledA, g') = shuffleTeam gen teamA
      (shuffledB, g'') = shuffleTeam g' teamB
      teamMatch = TeamMatchup (shuffledA, shuffledB)
      (singles1, g''') = singlesMatchup g'' teamA teamB
      ((doubles1, singles2, doubles2), g'''') = exhaustiveMatchSeq g''' teamA teamB
   in (Day2 teamMatch singles1 doubles1 singles2 doubles2, g'''')

makeDay3 :: (RandomGen g) => g -> Team -> Team -> (Day3, g)
makeDay3 gen teamA teamB =
  let (shuffledA, g') = shuffleTeam gen teamA
      (shuffledB, g'') = shuffleTeam g' teamB
      teamMatch = TeamMatchup (shuffledA, shuffledB)
      (singles1, g''') = singlesMatchup g'' teamA teamB
      ((doubles1, singles2, doubles2), g'''') = exhaustiveMatchSeq g''' teamA teamB
      (singles3, g''''') = singlesMatchup g'''' teamA teamB
   in (Day3 teamMatch singles1 doubles1 singles2 doubles2 singles3, g''''')

makeDay4 :: (RandomGen g) => g -> Team -> Team -> (Day4, g)
makeDay4 gen teamA teamB =
  let (Team a1 a2 a3 a4 a5, g') = shuffleTeam gen teamA
      (Team b1 b2 b3 b4 b5, g'') = shuffleTeam g' teamB
      (captainsPick, g''') = singlesMatchup g'' teamA teamB
   in ( Day4
          (SinglesMatchup (a1, b1))
          (SinglesMatchup (a2, b2))
          (SinglesMatchup (a3, b3))
          (SinglesMatchup (a4, b4))
          (SinglesMatchup (a5, b5))
          captainsPick,
        g'''
      )

makeSchedule :: (RandomGen g) => g -> Team -> Team -> Schedule
makeSchedule gen teamA teamB =
  let (day1, g') = makeDay1 gen teamA teamB
      (day2, g'') = makeDay2 g' teamA teamB
      (day3, g''') = makeDay3 g'' teamA teamB
      (day4, _) = makeDay4 g''' teamA teamB
   in Schedule day1 day2 day3 day4
