module Sim.IO (loadTeamFromJson, readResults, writeResults) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Match (Team (..))
import Results (SimSummary)

loadTeamFromJson :: FilePath -> IO (Either String Team)
loadTeamFromJson =
  eitherDecodeFileStrict

writeResults :: FilePath -> SimSummary -> IO ()
writeResults = encodeFile

readResults :: FilePath -> IO (Either String SimSummary)
readResults = eitherDecodeFileStrict
