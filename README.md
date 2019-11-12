# Mad Props

[Hackage & Docs](http://hackage.haskell.org/package/mad-props)

Mad props is a simple generalized propagator framework. This means it's pretty good at expressing and solving generalized [constraint satisfaction problems](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem).

Note that `mad-props` doesn't use lattice filters for propagation, nor does it yet support dynamic choice of propagator elements (though you can specify choice ordering through the container type you choose). Those things are more a bit more complicated.

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

Sudoku is a constraint satisfaction problem, the "constraints" are that each of the numbers 1-9 are represented in each row, column and 3x3 grid. 

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

We've now got our problem as a list of rows of 'cells', each cell is a set containing the possible numbers for that cell.

We need to express the constraint that each 'region' (i.e. row, column and 'block') can only have one of each number in them. We'll write some helper function for collecting the regions of the puzzle:

```haskell
rowsOf, colsOf, blocksOf :: [[a]] -> [[a]]
rowsOf = id
colsOf = transpose
blocksOf = chunksOf 9 . concat . concat . fmap transpose . chunksOf 3 . transpose
```

Now we can worry about telling the system about our constraints. 

We can now introduce the constraints of Sudoku as relations between cells. The cells in each region are related to one other in the sense that **their values must be disjoint**. No two cells in each quadrant can have the same value. 

```haskell
-- | Given a board of 'PVar's, link the appropriate cells with 'disjoint' constraints
linkBoardCells :: [[PVar S.Set Int]] -> Prop ()
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

This function introduces a few new types, namely `Prop` and `Pvar`. We'll show how `PVar`s are actually created soon, but the gist of this function is that we map over each 'region' and relate every variable to every other one.

`Prop` is a monad which allows us to create and link `PVar`s together. It keeps track of the constraints on all of our variables and will eventually build a graph that the library uses to solve the problem.

We call the `constrain` function to state that no cell pairing within a region should have the same number.

`constrain` accepts two `PVar`s and a function, the function takes a 'choice' from the first variable and uses it to constrain the 'options' from the second. In this case, if the first variable is fixed to a specific value we 'propagate' by removing all matching values from the other variable's pool, you can see the implementation of the `disj` helper above. The information about this constraint is stored inside the `Prop` monad.

Set disjunction is symmetric, propagators in general are not, so we'll need to 'constrain' in each direction. Luckily our loop will process each pair twice, so we'll run this once in each direction.

Here's the real signature in case you're curious: 

```haskell
constrain :: Monad m
          => PVar f a
          -> PVar g b
          -> (a -> g b -> g b)
          -> PropT m ()
```

We're almost there; we've got a way to constrain a board of `PVar`s, but we need to make the board of `PVar`s somehow!

This is pretty easy; we can make a `PVar` by calling `newPVar` and passing it a container full of possible options the variable could be. We'll convert our `[[S.Set Int]]` into `[[PVar S.Set Int]]` by traversing the structure using `newPVar`.

```haskell
-- | Given a sudoku board, apply the necessary constraints and return a result board of 'PVar's.
constrainBoard :: [[S.Set Int]]-> Prop [[PVar S.Set Int]]
constrainBoard board = do
    vars <- (traverse . traverse) newPVar board
    linkBoardCells vars
    return vars
```

Here's the signature of `newPVar` in case you're curious:

```haskell
newPVar :: (Monad m, Foldable f, Typeable f, Typeable a) 
        => f a 
        -> PropT m (PVar f a)
```

Now that we've got our problem set up we need to execute it!

```haskell
-- Solve a given sudoku board and print it to screen
solvePuzzle :: [[S.Set Int]] -> IO ()
solvePuzzle puz = do
    -- We know it will succeed, but in general you should handle failure safely
    let Just results = solve (fmap . fmap) $ constrainBoard puz
    putStrLn $ boardToText results
```

`solvePuzzle` will print a solution for any valid puzzle you pass it. It accepts a puzzle, builds and constrains the cells, then calls `solve` which will find a valid solution for the constraints we provided if possible. We pass it a 'finalizer' function which accepts a function for resolving any `PVar` to its 'solved' result. In our case we just use `fmap . fmap` to map the resolver over every PVar in the board returned from `constrainBoard`. If all went well we'll have the solution of each cell! Then we'll print it out.

Unfortunately `solve` has a bit of a complicated signature, there are simpler versions, but unfortunately they're not possible until GHC supports proper ImpredicativeTypes.

```haskell
solve :: forall a r.
        -- A finalizer which accepts a PVar 'resolver' as an argument
        -- alongside the result of the Prop setup, and returns some result
        ((forall f x. PVar f x -> x) -> a -> r)
      -> Prop a
      -> (Maybe r)
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
constrainQueens :: Int -> Prop [PVar S.Set Coord]
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
    let Just results = solve fmap (constrainQueens n)
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
