{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PlotSurvey.ScatterConfig where

import Chart (Place (..))
import qualified Chart as ChartSVG
import Control.Monad.Trans.State (runState)
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified DataFrame.Display.Web.Plot as DfPlot
import qualified DataFrame.Typed as DT
import ExampleData (LabeledDfSchema, labeledPointsDf)
import qualified Granite as G
import qualified Granite.Svg as GSvg
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Vega.VegaLite as V
import Optics.Core (set, (%), (.~))
import System.Random (mkStdGen)

sampleDf :: DT.TypedDataFrame LabeledDfSchema
sampleDf = fst $ runState (labeledPointsDf 100) (mkStdGen 23874)

dataframeScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
dataframeScatterConfig typedDf =
  let plotConfig =
        DfPlot.PlotConfig
          { DfPlot.plotWidth = 300,
            DfPlot.plotTitle = "Little plot",
            DfPlot.plotHeight = 300,
            DfPlot.plotFile = Nothing,
            DfPlot.plotType = DfPlot.Scatter
          }
   in DfPlot.plotScatterByWith "x" "y" "tag" plotConfig (DT.thaw typedDf)
        >>= ( \(DfPlot.HtmlPlot plotText) ->
                Text.writeFile "plots/dataframeScatterConfig.html" plotText
            )

graniteSvgScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
graniteSvgScatterConfig df =
  let xs = DT.columnAsList @"x" df
      ys = DT.columnAsList @"y" df
      plotConfig =
        G.defPlot
          { G.widthChars = 30,
            G.heightChars = 30,
            G.plotTitle = "Little plot",
            G.xBounds = (Just (-1), Just 2),
            G.yBounds = (Just (-0.5), Just 1.5),
            G.colorPalette = [G.BrightGreen, G.BrightBlack]
          }
      plot = GSvg.scatter [G.series "points" (zip xs ys)] plotConfig
   in Text.writeFile "plots/graniteSvgScatterConfig.html" plot

hvegaScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
hvegaScatterConfig df =
  let vegaColumns =
        [ V.dataColumn "x" (V.Numbers (DT.columnAsList @"x" df)),
          V.dataColumn "y" (V.Numbers (DT.columnAsList @"y" df)),
          V.dataColumn "tag" (V.Strings ((Text.pack . pure <$> DT.columnAsList @"tag" df)))
        ]
      vegaData = foldl' (.) (V.dataFromColumns []) vegaColumns
      enc =
        V.encoding
          . V.position
            V.X
            [ V.PName "x",
              V.PmType V.Quantitative,
              -- control axis extent
              V.PScale [V.SDomain (V.DNumbers [0, 1.2])],
              V.PAxis [V.AxTitle "The x values"]
            ]
          . V.position
            V.Y
            [ V.PName "y",
              V.PmType V.Quantitative,
              -- set a log scale on y
              V.PScale [V.SType V.ScLog],
              V.PAxis [V.AxTitle "The very important y values", V.AxTitleFontSize 18]
            ]
          -- color the points based on tag using the "purples" scale
          . V.color [V.MName "tag", V.MmType V.Nominal, V.MScale [V.SScheme "purples" [10]]]
      -- set a different title with a different font
      title = V.title "Wide purple plot >>=" [V.TFont "Hasklug Nerd Font", V.TFontStyle "italic"]
   in V.toHtmlFile "plots/vegaScatterConfig.html" $
        V.toVegaLite
          [ vegaData [],
            V.mark V.Point [],
            enc [],
            title,
            -- change the plot dimensions
            V.width 600,
            V.height 200,
            V.background "rgba(20, 0, 50, 0.2)"
          ]

chartSvgScatter :: DT.TypedDataFrame LabeledDfSchema -> IO ()
chartSvgScatter df =
  let xs = DT.columnAsList @"x" df
      ys = DT.columnAsList @"y" df
      points = zipWith ChartSVG.Point xs ys
      -- change mark color
      style = ChartSVG.defaultGlyphStyle & #color .~ ChartSVG.palette 123 & #size .~ 0.015
      chart = ChartSVG.GlyphChart style points
      scatterExample =
        mempty
          -- title a plot
          & set #chartTree (ChartSVG.named "titled-scatter" [chart])
          & #hudOptions
            .~ ( ChartSVG.defaultHudOptions
                   -- title a plot
                   & #titles
                     .~ [ ChartSVG.Priority 0 $
                            ChartSVG.defaultTitleOptions "<$> titled scatter <$>"
                              & #style % #size .~ 0.05,
                          -- add specific labels for x and y
                          ChartSVG.Priority 1 $
                            ChartSVG.defaultTitleOptions "x label" & #place .~ PlaceBottom,
                          ChartSVG.Priority 2 $
                            ChartSVG.defaultTitleOptions "y label"
                              & #place .~ PlaceLeft
                        ]
               )
          -- change font
          & #markupOptions % #cssOptions % #fontFamilies .~ "svg { font-family: \"Hasklug Nerd Font\"; }"
          -- resize the plot
          & #markupOptions % #markupHeight .~ Just 200
          & #markupOptions % #chartAspect .~ ChartSVG.FixedAspect 3 ::
          ChartSVG.ChartOptions
   in ChartSVG.writeChartOptions "plots/chartSvgScatterConfig.svg" scatterExample

chartScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
chartScatterConfig df =
  let xs = DT.columnAsList @"x" df
      -- log scale axis with LogValue newtype
      ys = Chart.LogValue <$> DT.columnAsList @"y" df
   in Cairo.toFile Chart.def "plots/chartScatterConfig.png" $ do
        -- change the color cycle
        -- easiest way to construct is with named colors in
        -- https://hackage.haskell.org/package/colour-2.3.6/docs/Data-Colour-Names.html
        Chart.setColors [Chart.opaque Chart.dodgerblue]
        -- title
        -- no ligature in the output for some reason?
        Chart.layout_title_style . Chart.font_name .= "Hasklug Nerd Font"
        Chart.layout_title .= "log y scatter >>="
        -- axis labels
        Chart.layout_x_axis
          . Chart.laxis_override
          .= Chart.axisLabelsOverride [(0.5, "x label")]
        Chart.layout_y_axis . Chart.laxis_override .= Chart.axisLabelsOverride [(0.5, "y label")]
        Chart.plot $ Chart.points "points" (zip xs ys)
