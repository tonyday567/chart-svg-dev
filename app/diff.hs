{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad
import Data.ByteString qualified as B
import Data.Function
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude
import Chart
import Chart.Examples
import Data.Algorithm.DiffOutput
import Data.Algorithm.Diff
import Data.ByteString.Char8 qualified as C
import MarkupParse
import Data.Bool
import Data.Bifunctor
import Control.Category ((>>>))

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
    ( testExample . (\(x,y) -> (y,x))
        <$> pathChartOptions    )

testExample :: (ChartOptions, FilePath) -> TestTree
testExample (co, fp) =
  goldenTest
    fp
    (B.readFile fp)
    (pure $ markdown_ Compact Xml $ markupChartOptions co)
    (\expected actual -> getDiff (C.lines expected) (C.lines actual) & fmap (bimap (C.unpack >>> pure) (C.unpack >>> pure)) & diffToLineRanges & prettyDiffs & (\xs -> bool (pure $ Just (show xs)) (pure Nothing) (xs==mempty)))
    (\_ -> pure ())

