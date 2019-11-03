module Main where

import Arma

main :: IO ()
main = do
  putStrLn "arma"

  armaRng_set_seed_random

  putStrLn "random seed set"

