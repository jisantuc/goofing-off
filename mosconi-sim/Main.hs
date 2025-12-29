{-# LANGUAGE OverloadedStrings #-}

module Main where

import Match (Player(..))

main :: IO ()
main = print . show $ Player "James Santucci" 553
