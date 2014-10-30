module Main where

import Debug.Trace
import Data.List

data ChessPiece = ChessPiece Char deriving Show

board w h = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

printBoard :: Int -> Int -> [(ChessPiece, (Int, Int))] -> String
printBoard w h pieces = unlines [[piece x y  | x <- [0 .. w - 1]] | y <- [h - 1, h - 2 .. 0 ]]
    where
        piece x y = case filter  (\(_, pos) -> pos == (x, y)) pieces of
                        (ChessPiece c, _):_ -> c
                        [] -> if even x /= even y then ' ' else '█'

(x1, y1) ♚ (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

(x1, y1) ♛ (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)

(x1, y1) ♜ (x2, y2) = x1 == x2 || y1 == y2

(x1, y1) ♝ (x2, y2) = abs (x1 - x2) == abs (y1 - y2)

(x1, y1) ♞ (x2, y2) = abs (x1 - x2) + abs (y1 - y2) == 3

filteredCombinations :: (a -> a -> Bool) -> Int -> [a] -> [a] -> [([a], [a])]
filteredCombinations hits count free preserved = go count free ((++) preserved) [] []
    where
        go 0 free   preserve prefix otherResults = (prefix, preserve free) : otherResults
        go _ []     preserve prefix otherResults = otherResults
        go n (x:xs) preserve prefix otherResults = go (n - 1) (notHitByX xs) (notHitByX . preserve) (x:prefix) withoutX
            where
                notHitByX = filter (not . hits x)
                withoutX  = go n xs ((:)x . preserve) prefix otherResults

multiCombinations :: (Show a) => [(a -> a -> Bool, Int)] -> [a] -> [[[a]]] -- внутренний - фигрура, средний - доска.
multiCombinations = go []
    where
        go placed [] _                          = [reverse placed]
        go placed _ []                          = []
        go placed ((hits, count):xs) unoccupied = concatMap (uncurry nextPiece) combinations
            where
                (preserved, safe) = partition (canHit placed) unoccupied
                combinations =  -- trace ("unoccupied: " ++ (show unoccupied) ++ "; placed: " ++ (show placed) ++ "; preserved:" ++ (show preserved) ++ "; safe:" ++ (show safe))
                                filteredCombinations hits count safe preserved
                nextPiece occupied remains =  -- trace ("occupied: " ++ (show occupied) ++ "; remains:" ++ (show remains)) 
                                                    (go (occupied:placed) xs remains)
                canHit occupied pos = or $ map (any $ hits pos) $ occupied

        secure :: placed -> free -> filter -> (safe, unsafe)
        secure = undefined

-- | The main entry point.
main :: IO ()
main = do
    -- print $ (0,2) ♜ (2,2)
    -- print $ filteredCombinations (♛) 1 (board 3 3) [(0,0)]
    -- print $ multiCombinations [((♚), 2), ((♜), 1)] (board 3 3)
    -- print $ length $ multiCombinations [((♜), 2), ((♞), 4)] (board 4 4)

    print $ length $ multiCombinations [((♛), 1), ((♜), 1), ((♝), 1), ((♚), 2), ((♞), 1)] (board 6 9)
    
    -- print $ multiCombinations [((♛), 4)] (board 4 4)
