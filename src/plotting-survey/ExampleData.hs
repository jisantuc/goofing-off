{-# LANGUAGE OverloadedStrings #-}

module ExampleData where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (StateT, state)
import qualified DataFrame as D
import System.Random (Random (random), RandomGen)

points :: (RandomGen g, Monad m) => Int -> StateT g m [Float]
points n = replicateM n . state $ random

pointsDf :: (RandomGen g, Monad m) => Int -> StateT g m D.DataFrame
pointsDf size = do
  x <- points size
  y <- points size
  pure $
    D.fromNamedColumns
      [ ("x", D.fromList x),
        ("y", D.fromList y)
      ]
