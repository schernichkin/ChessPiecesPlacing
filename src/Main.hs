module Main where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           System.CPUTime

{-
printBoard :: Int -> Int -> [(ChessPiece, (Int, Int))] -> String
printBoard w h pieces = unlines [[piece x y  | x <- [0 .. w - 1]] | y <- [h - 1, h - 2 .. 0 ]]
    where
        piece x y = case filter  (\(_, pos) -> pos == (x, y)) pieces of
                        (ChessPiece c, _):_ -> c
                        [] -> if even x /= even y then ' ' else '█'
-}

filteredCombinations :: (a -> a -> Bool) -> Int -> [a] -> [a] -> [([a], [a])]
filteredCombinations hits count free preserved = go count free ((++) preserved) [] []
    where
        go 0 remains preserve placed otherResults = (reverse placed, preserve remains) : otherResults
        go _ []             _      _ otherResults = otherResults
        go n (x:xs)  preserve placed otherResults = go (n - 1) (notHitByX xs) (notHitByX . preserve) (x:placed) withoutX
            where
                notHitByX = filter (not . hits x)
                withoutX  = go n xs ((:)x . preserve) placed otherResults

multiCombinations :: [(a -> a -> Bool, Int)] -> [a] -> [[[a]]] -- внутренний - фигрура, средний - доска.
multiCombinations = go []
    where
        go placed [] _                          = [reverse placed]
        go placed ((hits, count):xs) unoccupied = concatMap (uncurry nextPiece) combinations
            where
                (preserved, safe) = partition (canHit placed) unoccupied
                combinations = filteredCombinations hits count safe preserved
                nextPiece occupied remains = go (occupied:placed) xs remains
                canHit occupied pos = or $ map (any $ hits pos) $ occupied

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

        natural arg = case reads arg of
             [(r, "")] -> if (r > 0)
                            then return r
                            else fail "piece count must be greater than zero."
             _         -> fail $ "cannot parse value `" ++ arg ++ "'."

-- | Convert problem to a list of hit fuctions with count of pieces, list of positions and list of piece labels.
prepare :: Problem -> ([((Int, Int) -> (Int, Int) -> Bool, Int)], [(Int, Int)], [Char])
prepare problem = (hitFunctions, prepareBoard (width problem) (height problem), labels)
    where
        (hitFunctions, labels) = unzip $ catMaybes $ map
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

-- | The main entry point.
main :: IO ()
main = do
    problem <- execParser problemParser
    print problem
    let (hitFunctions, board, _) = prepare problem
        combinations = multiCombinations hitFunctions board
    res <- measure (length combinations)
    print $ fst res
    when (measureTime problem) (print $ snd res)

    -- print $ length $ multiCombinations [((♛), 1), ((♜), 1), ((♝), 1), ((♚), 2), ((♞), 1)] (board 6 9)
