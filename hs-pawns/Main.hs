{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.Map
import PathFinder
import Models
import Control.Monad

main :: IO ()
main = forM_ [1..5 :: Int] (\ _ -> do
  putStrLn $ show $ getResultResume $ stupidFind (3, 3) (8, 6) world
  putStrLn $ show $ getResultResume $ stupidFind2 (3, 3) (8, 6) world)

world :: World
world =
    (World (Field 10 empty) entities)
        where entities = fmap (\i -> (Position (7, i))) [3 .. 8]

