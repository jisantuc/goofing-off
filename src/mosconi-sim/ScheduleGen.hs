module ScheduleGen (makeSchedule) where

import Control.Monad.Trans.State (State, state)
import Match (Matchup, Team(..), Day1(..), Day2(..), Day3(..), Day4(..), teamToList)
import System.Random (RandomGen, randomR)
import Match (Schedule(..))
import Match (Matchup(..))
import Match (DoublesTeam(..))

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
        DoublesMatchup ((DoublesTeam a4' a5'), (DoublesTeam b4' b5'))
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
