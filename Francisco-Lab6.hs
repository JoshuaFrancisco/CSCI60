-- Lab 6: Tracing and Boolean warm-up

import Debug.Trace (trace)


---- Functions from earlier labs ----------------------------------------------

-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- insures: a^n â‰¡ b (mod m), 0 <= b < m
expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m = let (q,r) = divMod n 2
                   ans = expmod a q m
               in if r == 0 then ans*ans `mod` m else ans*ans*a `mod` m

{-
-- expmod Tracing Function (I tried :) )			   
expmod :: Integer -> Integer -> Integer -> Integer
expmod a n m | trace (show (a,n,m)) False = undefined
expmod a 0 m = 1
expmod a n m = let (q,r) = divMod n (fromIntegral b)
                   ans = expmod a q m
			   in trace ("--> " ++ show ans) ans
				in if r == 0 then ans*ans `mod` m else and*ans*a `mod` m
-}

{-
toBase :: Integer -> Integer -> [Integer]
toBase b n = reverse (toBase' b n) where
  toBase' :: Integer -> Integer -> [Integer]
  toBase' b 0 = []
  toBase' b n = let (q,r) = divMod n b
                in r : toBase' b q
-}

-- toBase Tracing Function 
toBase :: Integer -> Integer -> [Integer]
toBase b n | trace (show (b,n)) False = undefined
toBase b n = reverse (toBase b n) where
   toBase b n | trace (show (b,n)) False = undefined
   toBase b 0 = []
   toBase b n = let (q,r) = divMod n (fromIntegral b)
                    ans = (fromIntegral r) : toBase b q
                in trace ("--> " ++ show ans) ans

{- fromBase :: Integer -> [Integer] -> Integer
fromBase b xs = fromBase' b (reverse xs) where
  fromBase' :: Integer -> [Integer] -> Integer
  fromBase' b [] = 0
  fromBase' b (x:xs) = x + b * fromBase' b xs
-}

--fromBase Tracing Function
fromBase :: Integer -> [Integer] -> Integer
fromBase b xs | trace (show (b,xs)) False = undefined
fromBase b xs = fromBase b (reverse xs) where
  fromBase b xs | trace (show (b,xs)) False = undefined
  fromBase b [] = 0
  fromBase b (x:xs) = let ans = (fromIntegral x) + (fromIntegral b) * fromBase b xs
                      in trace ("--> " ++ show ans) ans

-- Tracing version of extGCD (from Piazza @81)
extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a b | trace (show (a,b)) False = undefined
extGCD a 0 = let ans = (a,1,0) in trace ("---> " ++ show ans) ans
extGCD a b = let (q,r) = divMod a b
                 (d,m,n) = extGCD b r
                 ans = (d,n,m-q*n)
             in trace ("---> " ++ show ans) ans


---- Part 1: Tracing ----------------------------------------------------------

-- Create tracing versions of expmod, toBase, and fromBase above
-- and cut and paste sample output here. Note that you will also need
-- to trace the helper functions toBase' and fromBase'.

 

{- Output of toBase 4 75
(4,75)
(4,75)
(4,18)
(4,4)
(4,1)
(4,0)
--> [1]
--> [0,1]
--> [2,0,1]
--> [3,2,0,1]
[1,0,2,3]
-}

{- Output of fromBase 5 [4,3,4,3]
(5,[4,3,4,3])
(5,[3,4,3,4])
(5,[4,3,4])
(5,[3,4])
(5,[4])
(5,[])
--> 4
--> 23
--> 119
--> 598
598
-}

---- Part 2: Boolean warm-up --------------------------------------------------

bools = [True, False]


-- We can get a list of all possible outputs of the not and && operations

not_vals = [not p | p <- bools]
and_vals = [p && q | p <- bools, q <- bools]

-- Do the same with the operators ||, <, <=, >, >=,  ==, and /=.

or_vals = [p || q | p <- bools, q <- bools]
less_vals = [p < q | p <- bools, q <- bools]
leq_vals = [p <= q  | p <- bools, q <- bools]
greater_vals = [p > q | p <- bools, q <- bools]
geq_vals = [p >= q | p <- bools, q <- bools]
eq_vals = [p == q | p <- bools, q <- bools]
neq_vals = [p /= q | p <- bools, q <- bools]


-- The expressions p && q and q && p always give the same result, which we
-- can check in two different ways:

and1 = [p && q | p <- bools, q <- bools] == [q && p | p <- bools, q <- bools]
and2 = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Find combinations of &&, ||, and not that give the same results as
-- each of the other operators above: <, <=, >, >=, ==, and /=, and write a
-- test like either and1 or and2 to check this (it should return True)

less_test = and [(p < q) == (not p && q) | p <- bools, q <- bools]
leq_test = leq_vals == [not p && q | p <- bools, q <- bools]
greater_test = greater_vals == [p > q | p <- bools, q <- bools]
geq_test = and [(p >= q) == (not p || q) | p <- bools, q <-bools]
eq_test = [p && q | p <- bools, q <- bools] == [q && p | q <- bools, p <- bools]
neq_test = neq_vals == [not p || q | p <- bools, q <- bools]