{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Text.IO as T
import DataFrame.Display.Web.Plot (defaultPlotConfig)
import qualified DataFrame.Display.Web.Plot as Plt
import Features

-- TODO:
-- [x] read in the json
-- [x] make a dataframe out of the results with USA racks won as a feature (probably just want bars of this)
-- [ ] accept argument for which summary json to read
main :: IO ()
main =
  let barPlotConfig = (defaultPlotConfig Plt.Bar)
   in do
        Right summary <- eitherDecodeFileStrict "wnt-ranked.json"
        let usaRacks = usaRacksWonSeries summary
        let usaRackCounts = usaRacksWonCount usaRacks
        Plt.HtmlPlot linePlot <-
          Plt.plotLinesWith "Team USA Racks Won" ["count"] barPlotConfig usaRackCounts
        -- (Plt.defaultPlotConfig Plt.Line)
        T.writeFile "lines-wnt.html" linePlot
