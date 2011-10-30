module StupidFind2 (stupidFind2) where

import BaseFind
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

type Marks s = STUArray s (Int, Int) Double

msquareMatrix :: Int -> Double -> ST s (Marks s)
msquareMatrix size value = newArray ((0, 0), (size -1, size -1)) value


type AccFinderResult s = (Marks s, [Point], Double, FinderResult)

-- | same as stupidFind but using ST monad
stupidFind2 :: Point -> Point -> World -> FinderResult
stupidFind2 orig dest world @ (World (Field size _) _) = {-# SCC "stupidFind2" #-}
    runST $ do
      marks              <- msquareMatrix size 9999999
      (_, _, _, results) <- recFind orig 0 (marks, [], 0, [])
      return results
    where

        recFind :: Point -> Double -> AccFinderResult s -> ST s (AccFinderResult s)
        recFind p cst (marks, path, cost, results) =
            do
              (newmarks, _, _, newresults) <- foldM makeStep (marks, (p : path), cst, results) stepCosts
              return (newmarks, path, cost, newresults)

        makeStep :: (AccFinderResult s) -> (Point, Double) -> ST s (AccFinderResult s)
        makeStep (marks, path @ (p : _), cost, results) (step, stepcost) =
            let pn @ (x, y) = add p step
                newcost     = cost + stepcost
            in if pn == dest
               then return (marks, path, cost, (reverse (pn : path), newcost) : results)
               else if isClean world pn
                    then do
                      oldcost <- readArray marks (x, y)
                      (if oldcost > newcost
                       then writeArray marks (x, y) newcost >> recFind pn newcost (marks, path, cost, results)
                       else return (marks, path, cost, results))
                    else return (marks, path, cost, results)