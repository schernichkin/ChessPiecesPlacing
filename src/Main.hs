module Main where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           System.CPUTime

-- | Generate all conbinations where hit function returns false for each pair of items.
-- Will also return list of unused items concatinated with preserved items which are not `hit' by
-- any of selected items
filteredCombinations :: (a -> a -> Bool) -> Int -> [a] -> [a] -> [([a], [a])]
filteredCombinations hits count free preserved = go count free (preserved ++) [] []
    where
        go 0 remains preserve placed otherResults = (reverse placed, preserve remains) : otherResults
        go _ []             _      _ otherResults = otherResults
        go n (x:xs)  preserve placed otherResults = go (n - 1) (notHitByX xs) (notHitByX . preserve) (x:placed) withoutX
            where
                notHitByX = filter (not . hits x)
                withoutX  = go n xs ((:)x . preserve) placed otherResults

-- | Generate all lists of combinations for supplied list of hit functions with items count and
-- list of free items. All items in the list do not hit any other items in the same list. Each list
-- contains same number of combinations in same order as in the hit function list.
multiCombinations :: [(a -> a -> Bool, Int)] -> [a] -> [[[a]]]
multiCombinations = go []
    where
        go placed [] _                    = [reverse placed]
        go placed ((hits, count):xs) free = concatMap (uncurry nextPiece) combinations
            where
                (preserved, safe) = partition (`canHit` placed) free
                combinations = filteredCombinations hits count safe preserved
                nextPiece occupied = go (occupied:placed) xs
                canHit = any . any . hits

-- | Problem definition
data Problem = Problem { width          :: Int
                       , height         :: Int
                       , kings          :: Maybe Int
                       , queens         :: Maybe Int
                       , rooks          :: Maybe Int
                       , bishops        :: Maybe Int
                       , knights        :: Maybe Int
                       , measureTime    :: Bool
                       , printSolutions :: Int
                       } deriving ( Show )

-- | Command line parser
problemParser :: ParserInfo Problem
problemParser =
    info (helper <*> parser)
         (  fullDesc
         <> progDesc "Calculate number of pieces arrangements on a WIDTH x HEIGHT chessboard." )
    where
        parser = Problem
             <$> argument auto ( metavar "WIDTH" )
             <*> argument auto ( metavar "HEIGHT" )
             <*> optional ( option natural ( long "king"   <> short 'k' <> help "Set number of kings"   <> metavar "INT" ) )
             <*> optional ( option natural ( long "queen"  <> short 'q' <> help "Set number of queens"  <> metavar "INT" ) )
             <*> optional ( option natural ( long "rook"   <> short 'r' <> help "Set number of rooks"   <> metavar "INT" ) )
             <*> optional ( option natural ( long "bishop" <> short 'b' <> help "Set number of bishops" <> metavar "INT" ) )
             <*> optional ( option natural ( long "knight" <> short 'n' <> help "Set number of knights" <> metavar "INT" ) )
             <*> flag False True ( long "time" <> short 't' <> help "Print time taken to calculate positions (in ms.)" )
             <*> option auto (long "pring" <> short 'p' <> help "Print INT solutions (specify -1 to print all solutions, default 0)" <> value 0 <> metavar "INT" )

        natural = do
          arg <- auto
          case arg of
             r -> if r > 0 then return r
                           else fail "piece count must be greater than zero."
             -- _         -> fail $ "cannot parse value `" ++ vvvv ++ "'."

-- | Convert problem to a list of hit fuctions with count of pieces, list of positions and list of piece labels.
prepare :: Problem -> ([((Int, Int) -> (Int, Int) -> Bool, Int)], [(Int, Int)], [Char])
prepare problem = (hitFunctions, prepareBoard (width problem) (height problem), labels)
    where
        (hitFunctions, labels) = unzip $ mapMaybe
               (\(count, hitFunc, label) -> fmap (\c -> ((hitFunc, c), label)) count)
               [ (queens problem,  queenHit,  'q')
               , (rooks problem,   rookHit,   'r')
               , (bishops problem, bishopHit, 'b')
               , (kings problem,   kingHit,   'k')
               , (knights problem, knightHit, 'n') ]

-- | Create list of positions for w * h chessboard
prepareBoard :: Int -> Int -> [(Int, Int)]
prepareBoard w h = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- | King hit function
kingHit :: (Int, Int) -> (Int, Int) -> Bool
kingHit (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- | Queen hit function
queenHit :: (Int, Int) -> (Int, Int) -> Bool
queenHit (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)

-- | Rook hit function
rookHit :: (Int, Int) -> (Int, Int) -> Bool
rookHit (x1, y1) (x2, y2) = x1 == x2 || y1 == y2

-- | Bishop hit function
bishopHit :: (Int, Int) -> (Int, Int) -> Bool
bishopHit (x1, y1) (x2, y2) = abs (x1 - x2) == abs (y1 - y2)

-- | Knight hit function
knightHit :: (Int, Int) -> (Int, Int) -> Bool
knightHit (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) == 3 && x1 /= x2 && y1 /= y2

-- Evaluate value to WHNF and return result with time elapsed in milliseconds
measure :: a -> IO (a, Integer)
measure x = do
    start <- getCPUTime
    result <- evaluate x
    end <- getCPUTime
    return (result, (end - start) `div` 10 ^ (9 :: Integer))

-- | Generate graphical board presentation in String format.
showBoard :: Int -> Int -> [(Char, (Int, Int))] -> String
showBoard w h pieces = unlines [[square x y  | x <- [0 .. w - 1]] | y <- [h - 1, h - 2 .. 0 ]]
    where
        square x y = case filter  (\(_, pos) -> pos == (x, y)) pieces of
                        (c, _):_ -> c
                        [] -> if even x /= even y then '.' else '*'

-- | The main entry point.
main :: IO ()
main = do
    problem <- execParser problemParser

    let (hitFunctions, board, labels) = prepare problem
        combinations = multiCombinations hitFunctions board

    res <- measure (length combinations)
    print $ fst res
    when (measureTime problem) (print $ snd res)

    let solutionsToPrint = case printSolutions problem of
                                c | c < 0     -> combinations
                                  | otherwise -> take c combinations
        printBoard = putStrLn . showBoard (width problem) (height problem) . concatMap (\(l, xs) -> map ((,)l) xs) . zip labels

    mapM_ printBoard solutionsToPrint
