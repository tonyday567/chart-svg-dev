{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad
import Data.ByteString qualified as B
import Data.Function
import Data.TreeDiff
import MarkupParse
import MarkupParse.Patch
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude
import Chart
import Chart.Examples

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
    (getMarkupFile Xml fp)
    (pure $ markupChartOptions co)
    (\expected actual -> pure (show . ansiWlEditExpr <$> patch expected actual))
    (\_ -> pure ())

getMarkupFile :: Standard -> FilePath -> IO Markup
getMarkupFile s fp = do
  bs <- B.readFile fp
  pure $ warnError $ markup s bs

-- round trip markdown >>> markup
isoMarkdownMarkup :: RenderStyle -> Standard -> Markup -> Markup
isoMarkdownMarkup r s m = m & (markdown r s >=> markup s) & warnError
