module Match where

import Data.Text (Text)
import System.Random.Stateful (RandomGen, randomR, runStateGen_)

data Player = Player
  { name :: Text,
    rating :: Int
  }
  deriving (Eq, Show)

data Team = Team Player Player Player Player Player deriving (Eq, Show)

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

-- Each player must play one singles match before playing again.
data Day4
  = Day4
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
      SinglesMatchup
  deriving (Show)

data Schedule
  = Schedule
      Day1
      Day2
      Day3
      Day4

-- Day 4

shuffleTeam :: (RandomGen g) => g -> Team -> (Team, g)
shuffleTeam gen (Team p1 p2 p3 p4 p5) =
  go gen [] [p1, p2, p3, p4, p5]
  where
    go g [p1', p2', p3', p4', p5'] _ = (Team p1' p2' p3' p4' p5', g)
    go g acc rest =
      -- randomR is a closed range, can't (!! length)
      let (idx, g') = randomR (0, length rest - 1) g
          selected = rest !! idx
       in go g' (acc <> [selected]) (filter (/= selected) rest)

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
makeDay2 gen teamA teamB = undefined

makeDay3 :: (RandomGen g) => g -> Team -> Team -> (Day3, g)
makeDay3 gen teamA teamB = undefined

makeDay4 :: (RandomGen g) => g -> Team -> Team -> (Day3, g)
makeDay4 gen teamA teamB = undefined

makeSchedule :: (RandomGen g) => g -> Team -> Team -> Schedule
makeSchedule gen teamA teamB =
  runStateGen_ gen $ undefined
