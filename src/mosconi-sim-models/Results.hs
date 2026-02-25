{-# LANGUAGE DeriveGeneric #-}

module Results
  ( MosconiTeam (..),
    MosconiResult (..),
    ScheduleSummary (..),
    SimSummary (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Match (Player, Schedule, Team)

data MosconiTeam = USA Team | Europe Team deriving (Eq, Generic, Show)

instance ToJSON MosconiTeam

instance FromJSON MosconiTeam

data MosconiResult = MosconiResult
  { winner :: MosconiTeam,
    losingTeamMatchesWon :: Int,
    playerRecords :: [(Player, Int, Int)]
  }
  deriving (Generic, Show)

instance ToJSON MosconiResult

instance FromJSON MosconiResult

data ScheduleSummary = ScheduleSummary
  { schedule :: Schedule,
    results :: [MosconiResult]
  }
  deriving (Generic)

instance ToJSON ScheduleSummary

instance FromJSON ScheduleSummary

data SimSummary = SimSummary
  { teamUsa :: Team,
    teamEurope :: Team,
    summaries :: [ScheduleSummary]
  }
  deriving (Generic)

instance ToJSON SimSummary

instance FromJSON SimSummary
