{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Features (usaRacksWon) where

import Data.Foldable (foldMap')
import qualified DataFrame as DataFrame
import Results (MosconiResult(..), MosconiTeam(..), ScheduleSummary (..), SimSummary (..))

usaRacksWon :: SimSummary -> DataFrame.DataFrame
usaRacksWon (SimSummary {summaries}) =
  DataFrame.fromNamedColumns $
    [ ( "Team USA Racks Won",
        DataFrame.fromList $
          foldMap'
            ( \(ScheduleSummary {results}) ->
                ( \case
                    MosconiResult {winner = Europe _, losingTeamMatchesWon} -> losingTeamMatchesWon
                    MosconiResult {} -> 11
                )
                  <$> results
            )
            summaries
      )
    ]
