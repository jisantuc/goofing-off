{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Features (usaRacksWonSeries, usaRacksWonCount) where

import Data.Foldable (foldMap')
import DataFrame ((|>))
import qualified DataFrame as D
import DataFrame.Functions (col)
import qualified DataFrame.Functions as F
import qualified DataFrame.Operations.Aggregation as Agg
import DataFrame.Operators ((.=))
import Results (MosconiResult (..), MosconiTeam (..), ScheduleSummary (..), SimSummary (..))


usaRacksWonSeries :: SimSummary -> D.DataFrame
usaRacksWonSeries (SimSummary {summaries}) =
  let colName = "Team USA Racks Won"
   in ( D.fromNamedColumns $
          [ ( colName,
              D.fromList $
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
      )

usaRacksWonCount :: D.DataFrame -> D.DataFrame
usaRacksWonCount df =
  let colName = "Team USA Racks Won"
      -- type application enables solving for the appropriate type of racksWonCol,
      -- which is important in the aggregate below
      racksWonCol = col @Int colName
   in df
        -- groupBy column because first column
        |> Agg.groupBy [colName]
        -- Text becomes name of aggregation result
        |> Agg.aggregate ["count" .= F.count racksWonCol]
        |> D.sortBy [D.Asc racksWonCol]
