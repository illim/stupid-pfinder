{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module StupidFind (stupidFind) where

import BaseFind
import Data.Array (Array, array, (//), (!))
import Data.Array.IArray (IArray)

type AccFinderResult a = (a (Int, Int) Double, [Point], Double, FinderResult)

squareMatrix :: Int -> Double -> Array (Int, Int) Double
squareMatrix size value = array ((0, 0), (size -1, size -1)) [ ((i, j), value) | i <- [0..size -1], j <- [0..size- 1] ]

-- | stupidFind (search all)
stupidFind :: Point -> Point -> World -> FinderResult
stupidFind orig dest world @ (World (Field size _) _) =  {-# SCC "stupidFind" #-}
  stupidFindImpl (squareMatrix size 9999999) orig dest world


-- TODO fix type signatures
-- stupidFindImpl :: forall a . IArray a Double => a (Int, Int) Double -> Point -> Point -> World -> FinderResult
stupidFindImpl init orig dest world  =
    let (_, _, _, results) = recFind orig 0 (init, [], 0, [])
    in results where

--        recFind :: Point -> Double -> AccFinderResult a -> AccFinderResult a
        recFind p cst (marks, path, cost, results) =
            let (newmarks, _, _, newresults) = foldl makeStep (marks, (p : path), cst, results) stepCosts
            in (newmarks, path, cost, newresults)

--        makeStep :: AccFinderResult a -> (Point, Double) -> AccFinderResult a
        makeStep (marks, path @ (p : _), cost, results) (step, stepcost) =
            let pn @ (x, y) = add p step
                newcost     = cost + stepcost
            in if pn == dest
               then (marks, path, cost, (reverse (pn : path), newcost) : results)
               else if isClean world pn && marks ! (x, y) > newcost
                    then
                        let newmarks = marks // [((x, y), newcost)]
                        in recFind pn newcost (newmarks , path, cost, results)
                    else (marks, path, cost, results)
