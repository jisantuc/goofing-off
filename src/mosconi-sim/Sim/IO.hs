{-# LANGUAGE LambdaCase #-}

module Sim.IO where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Functor ((<&>))
import Match (Team (..))

loadTeamFromJson :: FilePath -> IO (Either String Team)
loadTeamFromJson fp =
  eitherDecodeFileStrict fp <&> \case
    Right [p1, p2, p3, p4, p5] -> Right $ Team p1 p2 p3 p4 p5
    Right players ->
      let numPlayers = length players
       in if numPlayers < 5
            then Left "Too few players"
            else Left "Too many players"
    -- If players can't be decoded, return the underlying error
    Left e -> Left e
