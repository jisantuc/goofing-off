{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (optional)
import Control.Monad.Trans.State (runState)
import Match (makeSchedule)
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
    showDefault,
    str,
    value,
    (<**>),
  )
import Sim (MosconiTeam (..), runMosconi)
import Sim.IO (loadTeamFromJson)
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

seedParser :: Parser (Maybe Int)
seedParser = optional $ option auto (long "seed" <> metavar "SEED")

singleScheduleSimParser :: Parser MosconiSimConfig
singleScheduleSimParser =
  MosconiSimConfig
    <$> teamPathsParser
    <*> seedParser
    <*> (Single <$> numTrialsPerScheduleParser)

manySchedulesSimParser :: Parser MosconiSimConfig
manySchedulesSimParser =
  MosconiSimConfig
    <$> teamPathsParser
    <*> seedParser
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
    MosconiSimConfig {teamPaths = TeamPaths {teamUSAJsonFilePath, teamEuropeJsonFilePath}, seed, simMode} -> do
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
      -- TODO:
      -- branch on which sim mode
      -- get results
      -- write the output file (you know, whatever that looks like)
      let result =
            runState
              ( do
                  schedule <- makeSchedule teamUsa teamEurope
                  mosconiResult <- runMosconi (USA teamUsa) (Europe teamEurope) schedule
                  pure (schedule, mosconiResult)
              )
              stdGen
      print . snd . fst $ result
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
