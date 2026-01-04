{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sim (runMosconi, MosconiTeam (..), losingTeamMatchesWon) where

import Control.Monad.Trans.State.Lazy (State, state)
import Match
  ( DoublesTeam (..),
    Matchup (..),
    Player (..),
    Schedule,
    Team,
    scheduleToList,
    teamToList,
  )
import System.Random (RandomGen, randoms, split)

data MosconiTeam = USA Team | Europe Team deriving (Eq, Show)

data MosconiResult = MosconiResult
  { winner :: MosconiTeam,
    losingTeamMatchesWon :: Int
  }
  deriving (Show)

data SimState = SimState
  { aWins :: Int,
    bWins :: Int,
    teamA :: MosconiTeam,
    teamB :: MosconiTeam
  }

data PlayerRecord = PlayerRecord
  { singlesWins :: Int,
    singlesLosses :: Int,
    doublesWins :: Int,
    doublesLosses :: Int
  }
  deriving (Show)

newtype MatchupResult = MatchupResult
  {winningTeam :: MosconiTeam}

initSimState :: MosconiTeam -> MosconiTeam -> SimState
initSimState = SimState 0 0

winProbabilityForRatings :: (Floating a) => a -> a -> a
winProbabilityForRatings aRating bRating =
  1 / (1 + 10 ** ((bRating - aRating) / 400))

-- |
-- Calculate the win probability for a matchup. For singles matchups,
-- FargoRate says:
--
--  "When two players are 100 points apart, say a 300 versus a 400,
--  the ratio of game wins will be near 1:2, as in 5 games to 10 games, or 50 games to 100 games."
--
-- That's _pretty close_ to win probability with a scaling factor of 400 on Wikipedia's Elo page,
-- so I'm assuming I can use that as the FargoRate scaling factor.
--
-- I don't really know how to interpret pairs / sets of ratings. Is the rating for a team of 2 players
-- the arithmetic mean of the two players' ratings? Maybe! I don't know.
--
-- This function returns a list of probabilities, abusing laziness to make the probabilities work for
-- the five player team match.
perRackWinProbability :: Matchup -> [Double]
perRackWinProbability
  (SinglesMatchup (Player {rating = aRating}, Player {rating = bRating})) =
    repeat $ winProbabilityForRatings (fromIntegral aRating) (fromIntegral bRating)
perRackWinProbability (DoublesMatchup (DoublesTeam a1 a2, DoublesTeam b1 b2)) =
  let aRating = fromIntegral (rating a1 + rating a2) / 2
      bRating = fromIntegral (rating b1 + rating b2) / 2
   in repeat $ winProbabilityForRatings aRating bRating
-- reasonable test case -- a team matchup with identical ratings
-- has the same probability sequences as a singles / doubles matchup with identical ratings
perRackWinProbability (TeamMatchup (teamA, teamB)) =
  let aRatings = fromIntegral . rating <$> teamToList teamA
      bRatings = fromIntegral . rating <$> teamToList teamB
   in cycle $ zipWith winProbabilityForRatings aRatings bRatings

pickWinner :: (RandomGen g) => MosconiTeam -> MosconiTeam -> Matchup -> State g MatchupResult
pickWinner a b matchup = state $ \g ->
  let (genForThisMatch, genForRest) = split g
   in go genForRest (initSimState a b) (zip (perRackWinProbability matchup) (randoms genForThisMatch))
  where
    -- this is impossible, since the function is called with two infinite lists, but oh well
    go nextGenerator _ [] = (MatchupResult a, nextGenerator)
    go nextGenerator matchupState ((aWinProbability, dieRoll) : xs) =
      let nextState =
            if dieRoll < aWinProbability
              then
                matchupState {aWins = aWins matchupState + 1}
              else matchupState {bWins = bWins matchupState + 1}
          winner =
            if bWins nextState == 5
              then Just (teamB nextState)
              else if aWins nextState == 5 then Just (teamA nextState) else Nothing
       in case winner of
            Just t ->
              if t == a
                then
                  (MatchupResult t, nextGenerator)
                else (MatchupResult t, nextGenerator)
            Nothing ->
              go nextGenerator nextState xs

-- this is the go call with evaluating each match and deciding whether to stop/go
runMosconi :: (RandomGen g) => MosconiTeam -> MosconiTeam -> Schedule -> State g MosconiResult
runMosconi a b schedule =
  go (initSimState a b) (scheduleToList schedule)
  where
    go (SimState {aWins = 11, teamA, bWins}) _ = pure $ MosconiResult teamA bWins
    go (SimState {bWins = 11, teamB, aWins}) _ = pure $ MosconiResult teamB aWins
    -- also impossible I think? again, oh well
    go (SimState {teamA, teamB, aWins, bWins}) [] =
      let (winner', loserMatchupWins) = if aWins > bWins then (teamA, bWins) else (teamB, aWins)
       in pure $ MosconiResult winner' loserMatchupWins
    go (SimState {teamA, teamB, aWins, bWins}) (matchup : matchups) =
      do
        MatchupResult {winningTeam} <- pickWinner teamA teamB matchup
        let nextState = if winningTeam == teamA then (SimState {aWins = aWins + 1, bWins, teamA, teamB}) else (SimState {bWins = bWins + 1, aWins, teamA, teamB})
        go nextState matchups
