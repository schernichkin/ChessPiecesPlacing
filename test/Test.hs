{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Data.List
import           Main
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

hitFunctionsTest :: Test
hitFunctionsTest = testGroup "Pieces"
     [ testProperty "King attack reflexivity"   $ \(a :: (Int, Int)) b -> kingHit a b == kingHit b a
     , testProperty "Queen attack reflexivity"  $ \(a :: (Int, Int)) b -> queenHit a b == queenHit b a
     , testProperty "Rook attack reflexivity"   $ \(a :: (Int, Int)) b -> rookHit a b == rookHit b a
     , testProperty "Bishop attack reflexivity" $ \(a :: (Int, Int)) b -> bishopHit a b == bishopHit b a
     , testProperty "Knight attack reflexivity" $ \(a :: (Int, Int)) b -> knightHit a b == knightHit b a
     ]

filteredCombinationsTest :: Test
filteredCombinationsTest = testGroup "Combinations"
    [ testProperty "Correct number of combinations generated"
        $ forAll nk
        $ \(n, k) -> let combinations = filteredCombinations (const $ const False) (fromIntegral k) [1..n] []
                     in  length combinations == combinationCount n k
    , testProperty "All combinations unique"
        $ forAll nk
        $ \(n, k) -> let combinations = filteredCombinations (const $ const False) (fromIntegral k) [1..n] []
                     in  combinations == nub combinations
    , testProperty "All items in combinations unique"
        $ forAll nk
        $ \(n, k) -> let combinations = filteredCombinations (const $ const False) (fromIntegral k) [1..n] []
                     in  combinations ==  map (\(a, b) -> (nub a, nub b)) combinations
    ] where
        nk = do
            n <- choose (0, 11)
            k <- choose (0, n)
            return (n, k)

combinationCount :: Integral a => a -> a -> a
combinationCount _ 0 = 1
combinationCount 0 _ = 0
combinationCount n k = combinationCount (n-1) (k-1) * n `div` k

multiCombinationsTest :: Test
multiCombinationsTest =  testGroup "Multicombinations"
    [ testProperty "Correct number of multicombinations generated"
        $ forAll nks
        $ \(n, ks) -> let combinations = multiCombinations (map (\k -> (const $ const False, k)) ks) [1..n]
                      in  length combinations == multicombinationCount n ks
    , testProperty "All multicombinations unique"
        $ forAll nks
        $ \(n, ks) -> let combinations = multiCombinations (map (\k -> (const $ const False, k)) ks) [1..n]
                      in  combinations == nub combinations
    , testProperty "Combination count does not depend on order"
        $ forAll nks
        $ \(n, ks) -> let combinations1 = multiCombinations (map (\k -> (const $ const False, k)) ks) [1..n]
                          combinations2 = multiCombinations (map (\k -> (const $ const False, k)) $ reverse ks) [1..n]
                      in  length combinations1 == length combinations2
    ] where
        nks = do
            n <- choose (0, 9)
            l <- choose (0, n `div` 2)
            ks <- vectorOf l (choose (0, (n * 2) `div` (if l == 0 then 1 else l))) `suchThat` \ks -> sum ks <= n
            return (n, ks)

multicombinationCount :: Integral a => a -> [a] -> a
multicombinationCount _ []     = 1
multicombinationCount n (k:ks) = (combinationCount n k) * (multicombinationCount (n - k) ks)

wellKnownProblemsTest :: Test
wellKnownProblemsTest = testGroup "Well known problems"
    [ testCase "8 queens" $ length (multiCombinations [(queenHit, 8)] $ prepareBoard 8 8) @?= 92
    , testCase "4 bishops" $ length (multiCombinations [(bishopHit, 4)] $ prepareBoard 4 4) @?= 260
    , testCase "5 queens and 5 knights" $ length (multiCombinations [(queenHit, 5), (knightHit, 5)] $ prepareBoard 8 8) @?= 16
    , testCase "2 kings and 1 rook" $ length (multiCombinations [(kingHit, 2), (rookHit, 1)] $ prepareBoard 3 3) @?= 4
    , testCase "4 knights and 2 rooks" $ length (multiCombinations [(knightHit, 4), (rookHit, 2)] $ prepareBoard 4 4) @?= 8
    ]

main :: IO ()
main = defaultMain
       [ hitFunctionsTest
       , filteredCombinationsTest
       , multiCombinationsTest
       , wellKnownProblemsTest
       ]
