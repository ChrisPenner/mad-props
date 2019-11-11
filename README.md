# Mad Props

Mad props is a simple generalized propagator framework. This means it's pretty good at expressing and solving generalized [constraint satisfaction problems](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem).

There are many other constraint solvers out there, probably most of them are faster than this one, but for those who like the comfort and type-safety of working in Haskell, I've gotcha covered.

With other constraint solvers it can be a bit of a pain to express your problem; you either need to compress your problem down to relations between boolean variables, or try to cram your problem into their particular format. Mad Props uses a Monadic DSL for expressing the variables in your problem and the relationships between them, meaning you can use normal Haskell to express your problem.

It's still unfinished and undergoing rapid iteration and experimentation, so I wouldn't base any major projects on it yet.

## Example: Sudoku

We'll write a quick Sudoku solver using Propagators.

Here's a problem which Telegraph has claimed to be ["the world's hardest Sudoku"](https://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html). Let's see if we can crack it.

```haskell
hardestProblem :: [String]
hardestProblem = tail . lines $ [r|
8........
..36.....
.7..9.2..
.5...7...
....457..
...1...3.
..1....68
..85...1.
.9....4..|]
```

A Sudoku is a constraint satisfaction problem, the "constraints" are that each of the numbers 1-9 are represented in each row, column and 3x3 grid. `Props` allows us to create `PVars` a.k.a. Propagator Variables. `PVars` represent a piece of information in our problem which is 'unknown' but has some relationship with other variables. To convert Sudoku into a propagator problem we can make a new `PVar` for each cell, the `PVar` will contain either all possible values from 1-9; or ONLY the value which is specified in the puzzle. We use a Set to indicate the possibilities, but you can really use almost any container you like inside a `PVar`.

```haskell
txtToBoard :: [String] -> [[S.Set Int]]
txtToBoard = (fmap . fmap) possibilities
  where
    possibilities :: Char -> S.Set Int
    possibilities '.' = S.fromList [1..9]
    possibilities a = S.fromList [read [a]]

hardestBoard :: [[S.Set Int]]
hardestBoard = txtToBoard hardestProblem
```

This function takes our problem and converts it into a nested grid of variables! Each variable 'contains' all the possibilities for that square. Now we need to 'constrain' the problem!

We can then introduce the constraints of Sudoku as relations between these `PVars`. The cells in each 'quadrant' (i.e. square, row, or column) are each 'related' to one other in the sense that **their values must be disjoint**. No two cells in each quadrant can have the same value. We'll quickly write some shoddy functions to extract the lists of "regions" we need to worry about from our board. Getting the **rows** and **columns** is easy, getting the square **blocks** is a bit more tricky, the implementation here really doesn't matter.

```haskell
rowsOf, colsOf, blocksOf :: [[a]] -> [[a]]
rowsOf = id
colsOf = transpose
blocksOf = chunksOf 9 . concat . concat . fmap transpose . chunksOf 3 . transpose
```

Now we can worry about telling the system about our constraints. We'll map over each region relating every variable to every other one. This function assumes we've replaced the `Set a`'s in our board representation with the appropriate `PVar`'s, we'll actually do that soon, but for now you can look the other way.

```haskell
-- | Given a board of 'PVar's, link the appropriate cells with 'disjoint' constraints
linkBoardCells :: [[PVar (S.Set Int)]] -> Prop ()
linkBoardCells xs = do
    let rows = rowsOf xs
    let cols = colsOf xs
    let blocks = blocksOf xs
    for_ (rows <> cols <> blocks) $ \region -> do
        let uniquePairings = [(a, b) | a <- region, b <- region, a /= b]
        for_ uniquePairings $ \(a, b) -> constrain a b disj
  where
    disj :: Ord a => a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs
```


Now every pair of `PVars` in each region is linked by the `disj` relation.

`constrain` accepts two `PVar`s and a function, the function takes a 'choice' from the first variable and uses it to constrain the 'options' from the second. In this case, if the first variable is fixed to a specific value we 'propagate' by removing all matching values from the other variable's pool, you can see the implementation of the `disj` helper above. The information about the 'link' is stored inside the `Prop` monad.

Here's the real signature in case you're curious: 

```haskell
constrain :: (Monad m, Typeable g, Typeable (Element f)) 
          => PVar f -> PVar g -> (Element f -> g -> g) 
          -> PropT m ()
```

Set disjunction is symmetric, propagators in general are not, so we'll need to 'constrain' in each direction. Luckily our loop will process each pair twice, so we'll run this once in each direction.

Now we can link our parts together:

```haskell
-- | Given a sudoku board, apply the necessary constraints and return a result board of
-- 'PVar's. We wrap the result in 'Compose' because 'solve' requires a Functor over 'PVar's
constrainBoard :: [[S.Set Int]]-> Prop (Compose [] [] (PVar (S.Set Int)))
constrainBoard board = do
    vars <- (traverse . traverse) newPVar board
    linkBoardCells vars
    return (Compose vars)
```

We accept a sudoku "board", we replace each `Set Int` with a `PVar (S.Set Int)` using `newPVar` which creates a propagator from a set of possible values. This is a propagator variable which has a `Set` of Ints which the variable could take. We then link all the board's cells together using constraints, and lastly return a `Functor` full of `PVar`s; which will later be replaced with actual values. `Compose` converts a list of lists into a single functor over the nested elements.

```haskell
newPVar :: (Monad m, MonoFoldable f, Typeable f, Typeable (Element f)) 
        => f -> PropT m (PVar f)
```

Now that we've got our problem set up we need to execute it!

```haskell
-- Solve a given sudoku board and print it to screen
solvePuzzle :: [[S.Set Int]] -> IO ()
solvePuzzle puz = do
    -- We know it will succeed, but in general you should handle failure safely
    let Just (Compose results) = solve $ constrainBoard puz
    putStrLn $ boardToText results
```

We run `solveGraph` to run the propagation solver. It accepts a puzzle, builds and constrains the cells, then calls `solve` which maps over the `Compose`'d board we created in `constrainBoard` and replaces all the `PVar`s with actual results! If all went well we'll have the solution of each cell! Then we'll print it out.

Here are some types first, then we'll try it out:

```haskell
solve :: (Functor f, Typeable (Element g)) 
      => Prop (f (PVar g)) -> Maybe (f (Element g))
```

We can plug in our hardest sudoku and after a second or two we'll print out the answer!

```haskell
>>> solvePuzzle hardestBoard
812753649
943682175
675491283
154237896
369845721
287169534
521974368
438526917
796318452
```

You can double check it for me, but I'm pretty sure that's a valid solution!

## Example: N-Queens

Just for fun, here's the N-Queens problem

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Examples.NQueens where

import qualified Data.Set as S
import Props
import Data.Foldable
import Data.List

-- | A board coordinate
type Coord = (Int, Int)

-- | Given a number of queens, constrain them to not overlap
constrainQueens :: Int -> Prop [PVar (S.Set Coord)]
constrainQueens n = do
    -- All possible grid locations
    let locations = S.fromList [(x, y) | x <- [0..n - 1], y <- [0..n - 1]]
    -- Each queen could initially be placed anywhere
    let queens = replicate n locations
    -- Make a PVar for each queen's location
    queenVars <- traverse newPVar queens
    -- Each pair of queens must not overlap
    let queenPairs = [(a, b) | a <- queenVars, b <- queenVars, a /= b]
    for_ queenPairs $ \(a, b) -> require (\x y -> not $ overlapping x y) a b
    return queenVars

-- | Check whether two queens overlap with each other (i.e. could kill each other)
overlapping :: Coord -> Coord -> Bool
overlapping (x, y) (x', y')
  -- Same Row
  | x == x' = True
  -- Same Column
  | y == y' = True
  -- Same Diagonal 1
  | x - x' == y - y' = True
  -- Same Diagonal 2
  | x + y == x' + y' = True
  | otherwise = False

-- | Print an nQueens puzzle to a string.
showSolution :: Int -> [Coord] -> String
showSolution n (S.fromList -> qs) =
    let str = toChar . (`S.member` qs) <$> [(x, y) | x <- [0..n-1], y <- [0..n-1]]
     in unlines . chunksOf n $ str
  where
    toChar :: Bool -> Char
    toChar True = 'Q'
    toChar False = '.'

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf n = unfoldr go
      where
        go [] = Nothing
        go xs = Just (take n xs, drop n xs)

-- | Solve and print an N-Queens puzzle
nQueens :: Int -> IO ()
nQueens n = do
    let Just results = solve (constrainQueens n)
    putStrLn $ showSolution n results

-- | Solve and print all possible solutions of an N-Queens puzzle
-- This will include duplicates.
nQueensAll :: Int -> IO ()
nQueensAll n = do
    let results = solveAll (constrainQueens n)
    traverse_ (putStrLn . showSolution n) results
```

## Performance

This is a generalized solution, so performance suffers in relation to a tool built for the job (e.g. It's not as fast as dedicated Sudoku solvers); but it does "pretty well".
