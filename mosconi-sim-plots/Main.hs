{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Features
import DataFrame.Display.Web.Plot as Plt
import qualified Data.Text.IO as T

-- TODO:
-- [x] read in the json
-- make a dataframe out of the results with USA racks won as a feature (probably just want bars of this)
main :: IO ()
main = do
  Right summary <- eitherDecodeFileStrict "big.json"
  let usaRacks = usaRacksWon summary
  HtmlPlot plot <- Plt.plotSingleBars "Team USA Racks Won" (defaultPlotConfig Bar) usaRacks
  T.writeFile "bars.html" plot
