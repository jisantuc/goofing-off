-- # usage in Chart-svg styling / charting
{-# LANGUAGE OverloadedLabels #-}
-- String -> Text for DataFrame and other labels
{-# LANGUAGE OverloadedStrings #-}

module PlotSurvey.Scatter where

import qualified Chart as ChartSVG
import Control.Monad.Trans.State (runState)
import Data.Function ((&))
import qualified Data.Text.IO as Text
import qualified DataFrame as D
import qualified DataFrame.Display.Web.Plot as DfPlot
import ExampleData (pointsDf)
import qualified Granite as G
import qualified Granite.Svg as GSvg
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Easy ((.=))
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Vega.VegaLite as V
import Optics.Core (set, (.~))
import System.Random (mkStdGen)

sampleDf :: D.DataFrame
sampleDf = fst $ runState (pointsDf 100) (mkStdGen 123947)

dataframeScatter :: D.DataFrame -> IO ()
dataframeScatter df =
  DfPlot.plotScatter "x" "y" df
    >>= (\(DfPlot.HtmlPlot plot) -> Text.writeFile "plots/dataframeScatter.html" plot)

-- based on simplified example from the encoding docs:
-- https://hackage.haskell.org/package/hvega-0.12.0.7/docs/Graphics-Vega-VegaLite.html#v:encoding
hvegaScatter :: D.DataFrame -> IO ()
hvegaScatter df =
  let vegaColumns =
        ( \name ->
            V.dataColumn name (V.Numbers (D.extractNumericColumn name df))
        )
          <$> (D.columnNames df)
      vegaData = foldl' (.) (V.dataFromColumns []) vegaColumns
      enc =
        V.encoding
          . V.position V.X [V.PName "x", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "y", V.PmType V.Quantitative]
   in V.toHtmlFile "plots/vegaScatter.html" $
        V.toVegaLite
          [ vegaData [],
            V.mark V.Point [],
            enc []
          ]

-- based on https://github.com/timbod7/haskell-chart/wiki/example-10
chartScatter :: D.DataFrame -> IO ()
chartScatter df =
  let xs = D.extractNumericColumn "x" df
      ys = D.extractNumericColumn "y" df
   in Cairo.toFile Chart.def "plots/chartScatter.png" $ do
        Chart.layout_title .= "scatter"
        Chart.plot $ Chart.points "points" (zip xs ys)

-- based on lines example in usage https://hackage.haskell.org/package/chart-svg-0.8.3.2
-- with some guesswork around the right way to plot just some points instead of
-- lines
chartSvgScatter :: D.DataFrame -> IO ()
chartSvgScatter df =
  let xs = D.extractNumericColumn "x" df
      ys = D.extractNumericColumn "y" df
      points = zipWith ChartSVG.Point xs ys
      style = ChartSVG.defaultGlyphStyle & #color .~ ChartSVG.palette 0 & #size .~ 0.015
      chart = ChartSVG.GlyphChart style points
      scatterExample =
        mempty
          & set #chartTree (ChartSVG.named "scatter" [chart])
          & #hudOptions .~ ChartSVG.defaultHudOptions ::
          ChartSVG.ChartOptions
   in ChartSVG.writeChartOptions "plots/chartSvgScatter.svg" scatterExample

-- Requires `dataframe >= 0.5`
graniteSvgScatter :: D.DataFrame -> IO ()
graniteSvgScatter df =
  let xs = D.extractNumericColumn "x" df
      ys = D.extractNumericColumn "y" df
      plot = GSvg.scatter [G.series "points" (zip xs ys)] G.defPlot
   in Text.writeFile "plots/graniteSvgScatter.html" plot
