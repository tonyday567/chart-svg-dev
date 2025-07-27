{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Chart.Dev
  ( exampleTextChart
  , exampleText
  , exampleTextChart'
  , runG
  , rvs
  , rvsp
  , toCT
  , sameProjection
  , gradientChart
  , strip
  ) where

import NumHask.Prelude
import NumHask.Space
import qualified Prelude
import Chart
import Faker.Lorem
import Data.Text qualified as T
import Data.Text (Text)
import Optics.Core
import Control.Monad
import System.Random
import System.Random.Stateful
import System.Random.MWC.Distributions

exampleTextChart :: Int -> Int -> IO [Chart]
exampleTextChart r c = do
  ts <- fmap T.pack <$> replicateM r (unwords <$> replicateM c word)
  let s = defaultTextStyle & #textAnchor .~ AnchorStart
  pure $ zipWith (\t x -> TextChart s [(t, Point 0 x)]) ts [0..]

exampleText :: Int -> Int -> IO [Text]
exampleText r c =
  fmap T.pack <$> replicateM r (unwords <$> replicateM c word)

exampleTextChart' :: Int -> Int -> Style -> IO [Chart]
exampleTextChart' r c s = do
  ts <- fmap T.pack <$> replicateM r (unwords <$> replicateM c word)
  let s' = s & #textAnchor .~ AnchorStart
  pure $ zipWith (\t x -> TextChart s' [(t, Point 0 x)]) ts [0..]

-- | rvs creates a list of standard normal random variates.
--
rvs n g = replicateM n (standard g)

-- | rvsPair generates a list of correlated random variate tuples
--
-- > rvsp gen 3 0.8
-- [(1.8005943761746166e-2,7.074509906249835e-2),(0.36444481359059255,-0.7073208451897444),(-1.2939898115295387,-0.643930709405127)]
--
rvsp n c g = do
  s0 <- rvs n g
  s1 <- rvs n g
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zip s0 s1'

runG = runStateGen_ (mkStdGen 69)

toCT :: ChartOptions -> ChartTree
toCT co = view #chartTree $ forgetHud co

sameProjection :: ChartOptions -> (Bool, Maybe (Rect Double), Maybe (Rect Double))
sameProjection co = (ct'==ct'', view styleBox' ct', view styleBox' ct'')
    where
      asp = co & view (#markupOptions % #chartAspect)
      csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #chartTree co)
      viewbox = finalCanvas asp (Just csAndHud)
      ct' = projectChartTree viewbox csAndHud
      ct'' = set styleBox' (Just viewbox) csAndHud

{-
sameMulti :: ChartOptions -> (Bool, Maybe (Rect Double), Maybe (Rect Double))
sameMulti co = (ct'==ct'', ct', ct'')
    where
      asp = co & view (#markupOptions % #chartAspect)
      csAndHud = addHud (view (#markupOptions % #chartAspect) co) (view #hudOptions co) (view #chartTree co)
      viewbox = finalCanvas asp (Just csAndHud)
      ct' = view styleBox' $ set (styleBoxN' 10) (Just viewbox) csAndHud
      ct'' = view styleBox' $ set styleBox' (Just viewbox) csAndHud

    -}

gradientChart :: Int -> LCHA -> LCHA -> [Chart]
gradientChart grain c0 c1 =
  (\(r, c) -> RectChart (defaultRectStyle & set #color c & set #borderSize 0) [r])
    . (\x -> (Rect x (x + d) 0 1, view lcha2colour' (mixLCHA x c0 c1)))
    <$> grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

strip :: Rect Double -> Double -> Colour -> ChartTree
strip r w c =
        named
          "strip"
          [RectChart (defaultRectStyle & set #color transparent & set #borderSize w & set #borderColor c) [r]]
