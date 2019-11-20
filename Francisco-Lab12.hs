-- Lab 12: Partitions

-- Sets of Ints as lists. Invariant: sets as lists always has no duplicates
type Set = [Int]

-- Relations on sets of Ints. Invariant: no duplicates
type Rel = [(Int,Int)]

-- Universe (your code should work with any non-empty universe)
u = [1..8]

-- nub' or "give distinct elements of list" function
nub                     :: (Eq a) => [a] -> [a]
nub l                   = nub' l []
  where
    nub' [] _           = []
    nub' (x:xs) ls
        | x `elem` ls   = nub' xs ls
        | otherwise     = x : nub' xs (x:ls)

-- Helper functions
empty_set :: [[Int]] -> Bool
empty_set cs = or [null (ds) | ds <- cs] 

total :: [[Int]] -> Bool
total es = and [ or [x `elem` ds | ds <- es] | x <- u]

disjoint :: [[Int]] -> Bool
disjoint fs = and [ and [ not(x `elem` es)| x <- ds]| ds <- fs, es <- fs, es /= ds]

-- A partitition of u is a set of blocks, each of which is a set of elements
-- of u, which satisfies certain conditions (nontrivial, total, disjoint).
-- For example, here is the partitition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]


-- Write a function, part, that tests whether a list of lists is a partition
-- of u, including that each block is a set (i.e., has no duplicates)
part :: [[Int]] -> Bool
part bs = not(empty_set bs) && total bs && disjoint bs


-- Write a function, eq2part, that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u.
eq2part :: Rel -> [[Int]]
eq2part rs = nub [[ d | (c,d) <- rs, a == c] | (a,b) <- rs]


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u.
part2eq :: [[Int]] -> Rel
part2eq bs = [(x, y)| cs <- bs, x <- cs, y <- cs]


-- Write a function, kernel, that takes a function on u as input (given as
-- a list of pairs), and returns the associated equivalence relation on u.
-- You can assume that the input is really a function on u.
kernel :: [(Int,Int)] -> Rel
kernel rs = undefined