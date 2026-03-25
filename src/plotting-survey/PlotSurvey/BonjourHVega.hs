{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module PlotSurvey.BonjourHVega where

import Control.Monad.Trans.State (runState)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified DataFrame.Typed as DT
import ExampleData (LabeledDfSchema, labeledPointsDf)
import qualified Graphics.Vega.VegaLite as V
import System.Random (mkStdGen)
import Text.RawString.QQ (r)

-- rescale based on description in https://www.joshwcomeau.com/svg/interactive-guide-to-paths/
-- and use the quasiquote to get line breaks into the SVG so it's not totally inscrutable
-- or better yet just draw something by hand with the instructions
svgText :: Text
svgText =
  [r|M 0, 0
  h 0.5
  L 0,0.5
  v 1
  L -0.5,0
  Z|]

samplePointsDf :: Int -> DT.TypedDataFrame LabeledDfSchema
samplePointsDf numRows = fst $ runState (labeledPointsDf numRows) (mkStdGen 289351)

-- | Show an estimate of pi based on a bunch of randomly generated points.
--
-- This examples is like the Pi Monte Carlo example here:
-- https://vega.github.io/vega/examples/pi-monte-carlo/
hvegaPiMonteCarlo :: IO ()
hvegaPiMonteCarlo =
  let nRows = 10000
      points = samplePointsDf nRows

      -- map the columns from the dataframe into objects hvega expects
      rows =
        V.dataFromColumns []
          . V.dataColumn "idx" (V.Numbers (fromIntegral @Int <$> DT.columnAsList @"idx" points))
          . V.dataColumn "x" (V.Numbers (DT.columnAsList @"x" points))
          . V.dataColumn "y" (V.Numbers (DT.columnAsList @"y" points))

      -- add Vega data to each row for whether the point is inside
      -- or outside the unit circle, a rolling count of how many
      -- points are inside the unit circle, and an estimate of pi
      -- based on that count.
      randomPointsTransform =
        V.transform $
          -- surprising: the name of the field at this point is num_points_idx, the combination
          -- of the value from select and V.IRange
          ( V.filter (V.FCompose (V.Expr "num_points_idx >= datum.idx"))
              . V.calculateAs "datum.x * datum.x + datum.y * datum.y < 1 ? 1 : 0" "inside"
              . V.window [([V.WAggregateOp V.Sum, V.WField "inside"], "insideCount")] []
              . V.calculateAs "datum.insideCount * 4 / datum.idx" "piEstimate"
          )
            []

      -- color the points based on whether they're inside or outside the unit circle
      enc =
        V.encoding
          . V.position V.X [V.PName "x", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "y", V.PmType V.Quantitative]
          . V.color [V.MName "inside", V.MmType V.Nominal]
          . V.opacity [V.MNumber 0.15]
      -- slider to choose a value bound to "num_points_idx" (combination of the
      -- selection field name and slider field name, for reasons I don't really
      -- understand); used to filter for rows with idx < slider value, i.e. to choose
      -- how many points are used in the Monte Carlo estimation of pi
      slider = V.IRange "idx" [V.InMin 100, V.InMax (fromIntegral nRows), V.InStep 10]
      selection =
        V.selection
          . V.select
            "num_points"
            V.Single
            [V.Fields ["idx"], V.SInit [("idx", V.Number 1000)], V.Bind [slider]]
      -- transform data to select a single row (assuming idx is unique) to be able to use
      -- that row's piEstimate value
      pi_ = V.transform . V.filter (V.FCompose (V.Expr "num_points_idx == datum.idx"))
   in V.toHtmlFile "plots/vegaCalculatePi.html" $
        V.toVegaLite
          [ rows [],
            randomPointsTransform,
            V.layer
              [ V.asSpec
                  [ V.mark V.Point [V.MFilled True],
                    enc [],
                    selection []
                  ],
                V.asSpec
                  [ pi_ [],
                    V.mark V.Text [V.MFontSize 18, V.MFontWeight V.Bold],
                    V.encoding
                      . V.position V.X [V.PmType V.Quantitative, V.PDatum (V.Number 1)]
                      . V.position V.Y [V.PmType V.Quantitative, V.PDatum (V.Number 0.5)]
                      -- text lets you add a text mark somewhere on the image (annotation).
                      -- In this case I used a value derived from the data, but that's not
                      -- required.
                      . V.text
                        [ V.TName "piEstimate",
                          V.TFormatAsNum,
                          V.TFormat ".3f"
                        ]
                      $ []
                  ]
              ]
          ]

hvegaFacetAnnotatedMeans :: DT.TypedDataFrame LabeledDfSchema -> IO ()
hvegaFacetAnnotatedMeans df =
  let x = DT.columnAsList @"x" df
      y = DT.columnAsList @"y" df
      tag = DT.columnAsList @"tag" df
      vegaData =
        V.dataFromColumns []
          . V.dataColumn "x" (V.Numbers x)
          . V.dataColumn "y" (V.Numbers y)
          . V.dataColumn "tag" (V.Strings . (Text.pack . pure <$>) $ tag)
      means =
        V.transform
          . V.aggregate
            [ V.opAs V.Mean "x" "xBar",
              V.opAs V.Mean "y" "yBar"
            ]
            ["tag"]
      colorMarkProperties =
        [ V.MName "tag",
          V.MmType V.Nominal,
          V.MScale [V.SScheme "blues" [5]]
        ]
      enc =
        V.encoding
          . V.position V.X [V.PName "x", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "y", V.PmType V.Quantitative]
          . V.color colorMarkProperties

      meansEnc =
        V.encoding
          . V.position V.X [V.PName "xBar", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "yBar", V.PmType V.Quantitative]
          . V.shape [V.MSymbol $ V.SymPath svgText]
   in V.toHtmlFile "plots/hvegaSimpleFacet.html" $
        V.toVegaLite
          [ V.facetFlow [V.FName "tag", V.FmType V.Nominal],
            V.columns 4,
            vegaData [],
            V.specification . V.asSpec $
              [ V.layer
                  [ V.asSpec
                      [ V.mark V.Point [V.MFilled True],
                        enc []
                      ],
                    V.asSpec
                      [ means [],
                        meansEnc $ [],
                        (V.mark V.Point [V.MColor "firebrick"])
                      ]
                  ]
              ]
          ]
