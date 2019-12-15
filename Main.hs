{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Random

main :: IO ()
main = do
  let n = 1000000
  generateUniform n >>= analyze "uniform"
  generateUniform' n >>= analyze "exp"

binCount :: Int
binCount = 100000000

generateUniform :: Int -> IO [Double]
generateUniform n = sort <$> replicateM n (randomRIO (0, 1))

generateUniform' :: Int -> IO [Double]
generateUniform' n = go 0.0 where
  go :: Double -> IO [Double]
  go start
    | start >= 1 = return []
    | otherwise = do
      wait :: Double <- (* recip (fromIntegral n)) . negate . log <$> randomIO
      let newPoint = start + wait
      (newPoint :) <$> go newPoint


analyze :: String -> [Double] -> IO ()
analyze path points = do
  let n = length points
  let distances = zipWith (-) (tail points) points
      counted = Map.toAscList $ countBins distances
      bins = map (\(x, y) -> (fromIntegral x / fromIntegral binCount, fromIntegral y / fromIntegral n)) counted
      plot :: Plot Double Double
      plot = toPlot $ def
        & plot_lines_values .~ [bins]
      layout :: Layout Double Double
      layout = def
        & layout_plots .~ [plot]
        & layout_y_axis.laxis_generate .~ autoScaledLogAxis def
  print $ length counted
  void $ renderableToFile (FileOptions (1000, 1000) PNG) (path ++ ".png") (toRenderable layout)


  where
    bin :: Double -> Int
    bin v = floor $ v * fromIntegral binCount
    countBins :: [Double] -> Map Int Int
    countBins []     = Map.empty
    countBins (x:xs) = Map.alter (Just <$> maybe 1 (+1)) (bin x) $ countBins xs
