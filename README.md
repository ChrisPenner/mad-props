# Mad Props

Mad props is a simple generalized propagator framework. This means it's pretty good at expressing and solving generalized [constraint satisfaction problems](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem).

It's a pain to express CSP's to many solvers; you either need to compress your problem down to relations between boolean variables, or try to cram your problem into their particular format. Mad Props uses a Monadic DSL for expressing the variables in your problem and the relationships between them, meaning you can use normal Haskell to express your problem.

It's still unfinished and undergoing rapid iteration and experimentation, so I wouldn't base any major projects on it yet.

## Example: Sudoku

We'll write a quick Sudoku solver using Propagators.

Here's a problem which Telegraph has claimed to be ["the world's hardest Sudoku"](https://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html). Let's see if we can crack it.

```haskell
hardestBoard :: [String]
hardestBoard = tail . lines $ [r|
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
linkBoard :: [[PVar s (S.Set Int)]] -> GraphM s ()
linkBoard xs = do
    let rows = rowsOf xs
    let cols = colsOf xs
    let blocks = blocksOf xs
    for_ (rows <> cols <> blocks) $ \region -> do
        let uniquePairings = [(a, b) | a <- region, b <- region, a /= b]
        for_ uniquePairings $ \(a, b) -> link a b disj
  where
    disj :: Ord a => a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs
```


Now every pair of `PVars` in each region is linked by the `disj` relation.

`link` accepts two PVars and a function, the function takes a 'choice' from the first variable and uses it to constrain the 'options' from the second. In this case, if the first variable is fixed to a specific value we 'propagate' by removing all matching values from the other variable's pool, you can see the implementation of the `disj` helper above. The information about the 'link' is stored inside the `GraphM` monad.

Here's the real signature in case you're curious: 

```haskell
link :: (Typeable g, Typeable (Element f)) 
     => PVar s f -> PVar s g -> (Element f -> g -> g) -> GraphM s ()
```

Set disjunction is symmetric, propagators in general are not. However our loop will process each pair twice, once in each direction so we're still fine.

Now we can link our parts together:

```haskell
setup :: [[S.Set Int]]-> GraphM s [[PVar s (S.Set Int)]]
setup board = do
    vars <- (traverse . traverse) newPVar board
    linkBoard vars
    return vars
```

We accept a sudoku "board", we replace each `Set Int` with a `PVar s (S.Set Int)` using `newPVar` which creates a propagator from a set of possible values. This is a propagator variable which has a `Set` of Ints which the variable could take. The `PVar` and `GraphM` each take a scoping parameter `s` which helps prevent PVars from escaping the monad in which they were created. (Or at least it will once I implement that.)

```haskell
newPVar :: (MonoFoldable f, Typeable f, Typeable (Element f)) 
        => f -> GraphM s (PVar s f)
```

Now that we've got our problem set up we need to execute it!

```haskell
solvePuzzle :: [String] -> IO ()
solvePuzzle puz = do
    (vars, g) <- solveGraph (setup $ txtToBoard puz)
    let results = (fmap . fmap) (readPVar g) vars
    putStrLn $ boardToText results
```

We run `solveGraph` to run the propagation solver, which returns a solved graph and the result of the `GraphM`. For now we'll just return the grid of `PVars`, then use the graph and `readPVar` to extract the 'solved' value from them! If all went well we'll have the solution of each cell! Then we'll print it out.

Here are some types first, then we'll try it out:

```haskell
readPVar :: Typeable a 
         => Graph s -> PVar s f -> a
solveGraph :: GraphM s a -> IO (a, Graph s)
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

## Performance

This is a generalized solution, so performance suffers in relation to a tool built for the job (e.g. It's not as fast as dedicated sudoku solvers); but it does "pretty well". I'm still working on improving performance, but try it out and see how you fare.

