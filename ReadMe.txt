Chess problem

1. Answer

The answer is 20136752.

2. Solution description

2.1 Tools and language

I’ve decided to use Haskell because I know it sufficiently well to avoid possible technical issues. The program is written in quite pure way with almost no optimizations and relies only on basic data structures (lists mostly). I also use third party library for command line argument parsing, and several unit-testing libraries (all libraries available on Hackage). 

Program supplied with the Cabal file which can be used to build and test it with  GHC version 7.8.3

2.2 Algorithm

There is no well-known algorithm for arbitrary chess pieces placing, the program basically brute forcing. The computation complexity is very high, so it almost impossible to calculate solution for high dimensions. It takes about 10 second to solve given task (6x9 board with 2 Kings, 1 Queen, 1 Bishop, 1 Rook and 1 Knight) on my computer. Actually, the version I sending will take 15 seconds, but it also can print some of solutions, I found this feature nice and decided to sacrifice some performance to it.

To reduce search space the board is represented by list of positions. After placing any single piece all position which can be hit by current piece are removed from list. If there are more pieces to place but list of positions is empty search will fail. Program attempts to place pieces which hit most position firstly.

2.3 Implementation in details

The algorithm core consist or 2 functions: filteredCombinations and multiCombinations.

FilteredCombinations takes hit function (function of 2 parameters which returns true if one of positions can be hit from another), number of pieces need to be placed, list of free positions, and list of preserved positons (I will explain it meaning later). It returns list of pairs where first item is the list of pieces positions (number of positions is equal to number of pieces passed to function) and second item is the list of unoccupied position which is not hit by any of places pieces. It consists of unused free positions plus preserved position. List does not contain any positions which can be hit by places pieces.  If hit function constantly returns False, FilteredCombinations will just generate all distinct combinations of number of pieces from the set of positions.

FilteredCombinations is suitable for solving single piece placing problem (e.g. 8-queens problem), but not suitable for multiply types of pieces. This is what multiCombinations does. It takes list of hit functions with number of pieces associated with each function, list of positions and return list of all possible placements. Each placement is itself list – it contains list for picked positions for each piece. Thus list solutions contains all possible placements, each placement contains lists of positions for each piece type. Positions returned in same order as hit functions was passed, it make possible correctly associate pieces with positions.

MultiCombinations uses filteredCombinations internally to generate combinations for each hit function. It also use small helper function to split free position to “safe” and “preserved” lists. “Safe” list contains all positions from those current hit function can’t hit any of already placed pieces. This list passed filteredCombinations function as free positions list. “Preserved” positions can’t be used to place current pieces, because some of already placed pieces can be hit from it. But it possibly can be used by other piece type. This is the meaning of preserved argument of filteredCombinations function. FilteredCombinations does not use preserved positions for piece placement, but applies filter to it and appends to unused positions in its result to make these positions available for placement of other pieces.

All other functions are just helpers. There are command line argument parser; function which converts problem definition to form suitable for calling multiCombinations function; function which prints solution and hit function for each piece type.

3. Tests

Test are quite self-explanatory. There are some tests for hit function reflexivity (the only requirement for hit functions), some to test that correct number of combinations generated using combinatorics formulas, and some tests for well-know chess problems (8-quenns ect.) Dimensions intentionally picked small enough to make possible running tests in GHC interpreter.