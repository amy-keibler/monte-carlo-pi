module Main where

import System.Random

import MC.Pi (approximatePi)

numSamples :: Int
numSamples = 1000000

main :: IO ()
main = do
  gen <- getStdGen
  print $ approximatePi $ take numSamples $ randoms gen
