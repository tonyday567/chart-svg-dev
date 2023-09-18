{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib
  ( exampleTextChart
  , exampleText
  , exampleTextChart')
where

import Prelude
import Chart
import Faker.Lorem
import Data.Text qualified as T
import Data.Text (Text)
import Optics.Core
import Control.Monad

exampleTextChart :: Int -> Int -> IO [Chart]
exampleTextChart r c = do
  ts <- fmap T.pack <$> replicateM r (unwords <$> replicateM c word)
  let s = defaultTextStyle & #anchor .~ AnchorStart
  pure $ zipWith (\t x -> TextChart s [(t, Point 0 x)]) ts [0..]

exampleText :: Int -> Int -> IO [Text]
exampleText r c =
  fmap T.pack <$> replicateM r (unwords <$> replicateM c word)

exampleTextChart' :: Int -> Int -> TextStyle -> IO [Chart]
exampleTextChart' r c s = do
  ts <- fmap T.pack <$> replicateM r (unwords <$> replicateM c word)
  let s' = s & #anchor .~ AnchorStart
  pure $ zipWith (\t x -> TextChart s' [(t, Point 0 x)]) ts [0..]
