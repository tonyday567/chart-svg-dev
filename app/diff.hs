{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Chart
import Chart.Examples
import Circuit.Markup
import Control.Category ((>>>))
import Control.Monad
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Bifunctor
import Data.Bool
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Function
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ goldenTests
      ]

goldenTests :: TestTree
goldenTests =
  testGroup
    "examples"
    ( testExample . (\(x, y) -> (y, x))
        <$> pathChartOptions
    )

testExample :: (ChartOptions, FilePath) -> TestTree
testExample (co, fp) =
  goldenTest
    fp
    (B.readFile fp)
    (pure $ markdown_ Compact Xml $ markupChartOptions co)
    (\expected actual -> getDiff (C.lines expected) (C.lines actual) & fmap (bimap (C.unpack >>> pure) (C.unpack >>> pure)) & diffToLineRanges & prettyDiffs & (\xs -> bool (pure $ Just (show xs)) (pure Nothing) (xs == mempty)))
    (\_ -> pure ())
