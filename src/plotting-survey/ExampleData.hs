{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExampleData where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (StateT, state)
import qualified DataFrame as D
import qualified DataFrame.Typed as DT
import System.Random (RandomGen, random, randomR)

points :: (RandomGen g, Monad m) => Int -> StateT g m [Double]
points n = replicateM n . state $ random

labels :: (RandomGen g, Monad m) => Int -> StateT g m [Char]
labels n = replicateM n . state $ randomR ('a', 'j')

pointsDf :: (RandomGen g, Monad m) => Int -> StateT g m D.DataFrame
pointsDf size = do
  x <- points size
  y <- points size
  pure $
    D.fromNamedColumns
      [ ("x", D.fromList x),
        ("y", D.fromList y)
      ]

type LabeledDfSchema = '[DT.Column "x" Double, DT.Column "y" Double, DT.Column "tag" Char, DT.Column "idx" Int]

labeledPointsDf :: (RandomGen g, Monad m) => Int -> StateT g m (DT.TypedDataFrame LabeledDfSchema)
labeledPointsDf size = do
  x <- points size
  y <- points size
  tag <- labels size
  pure $
    DT.unsafeFreeze @LabeledDfSchema $
      D.fromNamedColumns
        [ ("x", D.fromList x),
          ("y", D.fromList y),
          ("tag", D.fromList tag),
          ("idx", D.fromList @Int $ (fst <$>) $ zip [1 ..] x)
        ]
