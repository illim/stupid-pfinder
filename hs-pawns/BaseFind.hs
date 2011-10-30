{-# LANGUAGE ExistentialQuantification #-}

module BaseFind(FinderResult,
                stepCosts,
                getResultResume,
                module Models) where

import Models
import Data.Ord   (comparing)
import Data.List  (minimumBy)

type FinderResult = [([Point], Double)]

stepCosts :: [(Point, Double)]
stepCosts = ortho ++ diag
    where ortho = fmap (\step -> (step, 1))  [(1, 0), (0, 1), (0, -1), (-1, 0)]
          diag  = fmap (\step -> (step, 1.414213)) [(1, 1) , (1, -1) , (-1, 1) , (-1, -1)]


getResultResume :: FinderResult -> (([Point], Double), Int)
getResultResume result =
    (best, count)
    where best  = minimumBy compareScore result
          count = length result

compareScore :: forall a b . Ord b => (a, b) -> (a, b) -> Ordering
compareScore x y = comparing snd x y