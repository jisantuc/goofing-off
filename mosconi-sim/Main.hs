{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (optional)
import Control.Monad (replicateM)
import Control.Monad.Trans.State (evalState)
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    str,
    value,
    (<**>),
  )
import Results
  ( MosconiTeam (..),
    ScheduleSummary (..),
    SimSummary (..),
  )
import ScheduleGen (makeSchedule)
import Sim (runMosconi)
import Sim.IO (loadTeamFromJson, writeResults)
import System.Random (getStdGen, mkStdGen)

data SimMode
  = Single {trials :: Int}
  | Many {trials :: Int, schedules :: Int}
  deriving (Show)

data TeamPaths = TeamPaths
  { teamUSAJsonFilePath :: FilePath,
    teamEuropeJsonFilePath :: FilePath
  }
  deriving (Show)

data MosconiSimConfig = MosconiSimConfig
  { teamPaths :: TeamPaths,
    seed :: Maybe Int,
    outFile :: FilePath,
    simMode :: SimMode
  }
  deriving (Show)

teamPathsParser :: Parser TeamPaths
teamPathsParser =
  TeamPaths
    <$> option str (long "usa-json" <> metavar "USA_JSON_PATH")
    <*> option str (long "europe-json" <> metavar "EUROPE_JSON_PATH")

numTrialsPerScheduleParser :: Parser Int
numTrialsPerScheduleParser =
  option
    auto
    ( long "trials"
        <> metavar "TRIAL_COUNT"
        <> value 1000
        <> showDefault
    )

outFileParser :: Parser FilePath
outFileParser =
  option str (long "out" <> short 'o' <> metavar "OUTPUT_FILE")

seedParser :: Parser (Maybe Int)
seedParser = optional $ option auto (long "seed" <> metavar "SEED")

singleScheduleSimParser :: Parser MosconiSimConfig
singleScheduleSimParser =
  MosconiSimConfig
    <$> teamPathsParser
    <*> seedParser
    <*> outFileParser
    <*> (Single <$> numTrialsPerScheduleParser)

manySchedulesSimParser :: Parser MosconiSimConfig
manySchedulesSimParser =
  MosconiSimConfig
    <$> teamPathsParser
    <*> seedParser
    <*> outFileParser
    <*> ( Many
            <$> numTrialsPerScheduleParser
            <*> option
              auto
              ( long "num-schedules"
                  <> metavar "NUM_SCHEDULES"
                  <> value 100
                  <> showDefault
              )
        )

-- options:
-- --output FILE to write results to a file; where "results" is something like schedule hash + USA win rate + USA mean racks won
-- maybe want full output in sqlite or something; sqlite would be nice for
-- including some interactive visualization in a blog, but also chonkier 🤔
main :: IO ()
main =
  execParser opts >>= \case
    MosconiSimConfig {teamPaths = TeamPaths {teamUSAJsonFilePath, teamEuropeJsonFilePath}, outFile, seed, simMode} -> do
      teamUsaParseResult <- loadTeamFromJson teamUSAJsonFilePath
      teamUsa <-
        either
          (\err -> fail ("Could not parse Team USA: " <> err))
          (\t -> pure t)
          teamUsaParseResult
      teamEuropeParseResult <- loadTeamFromJson teamEuropeJsonFilePath
      teamEurope <-
        either
          (\err -> fail ("Could not parse Team Europe: " <> err))
          (\t -> pure t)
          teamEuropeParseResult
      stdGen <- maybe getStdGen (pure . mkStdGen) seed
      let (nTrials, nSchedules) = case simMode of
            Single {trials} -> (trials, 1)
            Many {trials, schedules} -> (trials, schedules)
      let results =
            SimSummary teamUsa teamEurope $
              ( evalState
                  ( replicateM nSchedules $ do
                      schedule <- makeSchedule teamUsa teamEurope
                      mosconiResults <- replicateM nTrials $ runMosconi (USA teamUsa) (Europe teamEurope) schedule
                      pure (ScheduleSummary schedule mosconiResults)
                  )
                  stdGen
              )
      writeResults outFile results
  where
    cmdParser =
      hsubparser
        ( command "single" (info singleScheduleSimParser (progDesc "Run a sim for a single schedule"))
            <> command "many" (info manySchedulesSimParser (progDesc "Run sims for many random schedules"))
        )
    opts =
      info
        (cmdParser <**> helper)
        ( fullDesc
            <> progDesc "Simulate Mosconi outcomes based on 2025 rules for whatever teams you can imagine"
        )
