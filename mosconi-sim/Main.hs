{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    str,
    value,
    (<**>),
  )

data SimCount
  = Single {seed :: Maybe Int, trials :: Int}
  | Many {seed :: Maybe Int, trials :: Int, schedules :: Int}
  deriving (Show)

data MosconiSimConfig = MosconiSimConfig
  { teamUSAJsonFilePath :: FilePath,
    teamEuropeJsonFilePath :: FilePath,
    simCount :: SimCount
  }
  deriving (Show)

-- TODO: I need to separate the parsers better with _subcommands_ instead,
-- e.g. mosconi-sim single --seed 1234 --trials 1234
--   vs.
-- mosconi-sim many --seed 1234 --trials 1234 --schedules 1234
simCountParser :: Parser SimCount
simCountParser =
  let trialsOpt = option auto (long "trials" <> metavar "TRIAL_COUNT" <> value 1000)
      seedOpt = option auto (long "seed" <> metavar "SEED" <> value Nothing)
   in ( Many
          <$> seedOpt
          <*> trialsOpt
          <*> option auto (long "schedules" <> metavar "SCHEDULE_COUNT")
      )
        <|> (Single <$> seedOpt <*> trialsOpt)

configParser :: Parser MosconiSimConfig
configParser =
  MosconiSimConfig
    <$> option str (long "usa" <> metavar "USA_JSON_PATH")
    <*> option str (long "europe" <> metavar "EUROPE_JSON_PATH")
    <*> simCountParser

-- options:
-- --single [seed] to run one sim
-- --many [how many|100] for lots of sims
-- --output FILE to write results to a file (just winner/score for now)
-- some way of configuring players/ratings
main :: IO ()
main =
  execParser opts >>= print
  where
    opts =
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc "Simulate Mosconi outcomes based on 2025 rules for whatever teams you can imagine"
        )
