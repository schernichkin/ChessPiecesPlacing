module Optimised where

import           System.CPUTime
import           Control.Exception

filteredCombinations1 :: (a -> a -> Bool) -> Int -> [a] -> ([a] -> [a]) -> [([a], [a])]
filteredCombinations1 hits count free preserved = go count free preserved [] []
    where
        go 0 remains preserve placed otherResults = (reverse placed, preserve remains) : otherResults
        go _ []             _      _ otherResults = otherResults
        go n (x:xs)  preserve placed otherResults = go (n - 1) (notHitByX xs) (notHitByX . preserve) (x:placed) withoutX
            where
                notHitByX = filter (not . hits x)
                withoutX  = go n xs ((:)x . preserve) placed otherResults

partitionC :: (a -> Bool) -> [a] -> ([a] -> [a], [a] -> [a])
partitionC f xs = go xs (id, id)
    where
        go [] r          = r
        go (x:xs) (l, r) = if (f x) then go xs (\xs -> x:(l xs), r)
                                    else go xs (l, \xs -> x:(r xs))

multiCombinations1 :: [(a -> a -> Bool, Int)] -> [a] -> [[[a]]]
multiCombinations1 = go []
    where
        go placed [] _                    = [reverse placed]
        go placed ((hits, count):xs) free = concatMap (uncurry nextPiece) combinations
            where
                (preserved, safe) = partitionC (`canHit` placed) free
                combinations = filteredCombinations1 hits count (safe []) preserved
                nextPiece occupied = go (occupied:placed) xs
                canHit = any . any . hits


{-
partition               :: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)
-}

-- Evaluate value to WHNF and return result with time elapsed in milliseconds
measure :: a -> IO (a, Integer)
measure x = do
    start <- getCPUTime
    result <- evaluate x
    end <- getCPUTime
    return (result, (end - start) `div` 10 ^ (9 :: Integer))

main :: IO ()
main = do
    let (l, r) = partitionC (\x -> x < 5) [0..10]
    print (l [], r [])
        
    --b <- measure $ length $ filteredCombinations (const $ const False) 6 [1..20::Int] id
    --print $ b
    return ()