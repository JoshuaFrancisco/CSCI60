-- CSci 60 Lab 7

bools = [True, False]

-- Similar to one of the answers from Lab 6, the following code exhaustively
-- checks whether not(P) \/ Q is equivalent to P -> Q.
not_equiv = and [(not p || q) == (p <= q) | p <- bools, q <- bools]


-- Write similar defintions that check each of the following equivalences:

-- P /\ Q = Q /\ P                           and is commutative
equiv1 = and [(p && q) == (q && p) | p <- bools, q <- bools]
-- P \/ Q = Q \/ P                           or is commutative
equiv2 = and [( p || q) == (q || p) | p <- bools, q <- bools]
-- P /\ (P -> Q) = P /\ Q
equiv3 = and [(p && (p <= q)) == (p && q) | p <- bools, q <- bools]
-- P -> (P -> Q) = P -> Q
equiv4 = and [( p <= (p <= q)) == (p <= q) | p <- bools, q <- bools]

-- P /\ (Q /\ R) = (P /\ Q) /\ R             and is associative
equiv5 = and [(p && (q && r)) == ((p && q)&& r) | p <- bools, q <- bools, r<-bools]
-- P \/ (Q \/ R) = (P \/ Q) \/ R             or is associative
equiv6 = and [(p || (q || r)) == ((p || q) || r) | p <- bools, q <- bools, r<-bools]
-- P /\ (Q \/ R) = (P /\ Q) \/ (P /\ R)      and distributes over or
equiv7 = and [(p && (q || r)) == ((p && q) || (p && r)) | p <- bools, q <- bools, r<-bools]
-- P \/ (Q /\ R) = (P \/ Q) /\ (P \/ R)      or distributes over and
equiv8 = and [( p || (q && r )) == ((p || q) && (p || r)) | p <- bools, q <- bools, r<-bools]

-- P -> (Q /\ R) = (P -> Q) /\ (P -> R)      implies distributes over and
equiv9 = and [(p <= (q && r)) == ((p <= q) && (p<=r)) | p <- bools, q <- bools, r<-bools]
-- (P \/ Q) -> R = (P -> R) /\ (Q -> R)
equiv10 = and [((p || q)<=r) == ((p <= r) && (q<=r))  | p <- bools, q <- bools, r<-bools]
-- P -> (Q -> R) = (P /\ Q) -> R
equiv11 = and [(p <= (q<=r)) == ((p && q)<=r) | p <- bools, q <- bools, r<-bools]

-- The exclusive-or (xor) operation is equivalent to the /= operator in Haskell
-- Which of the following properties of exclusive-or are true? Answer each by
-- supplying Haskell code to check.

-- xor is commutative
equiv12 = and [(p /= q) == (q /= p) | p <- bools, q <- bools]
-- xor is associative
equiv13 = and [(p /= (q /= r)) == ((p /= q)/= r) | p <- bools, q <- bools, r<-bools]
-- xor distributes over and
equiv14 = and [( p /= (q && r )) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r<-bools]
-- xor distributes over or
equiv15 = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r<-bools]
-- and distributes over xor
equiv16 = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r<-bools]
-- or distributes over xor
equiv17 = and [( p || (q /= r )) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r<-bools]
-- implies distributes over xor
equiv18 = and [(p <= (q /= r)) == ((p <= q) /= (p<=r)) | p <- bools, q <- bools, r <-bools]



-- Translate each of the statements below, first, in a comment after "A: ",
-- into a logical statement involving forall, exists, /\, \/, ->, and not,
-- and then into Haskell code that checks ("brute force") whether the
-- statement is true. The universe of discourse in each case is u. Your
-- code should work with any universe u (try out several!), but here is
-- a particular one you can use (the order of elements shouldn't matter,
-- since both `and` and `or` are commutative):

u = [13,4,12,22,9,1,2,17,5]

-- I'll work the first example in two different ways

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) -> (n > 1)
prob1  = and [(n > 2) <= (n > 1) | n <- u]    -- direct solution
prob1' = and [n > 1 | n <- u, n > 2]          -- using a bounded quantifier

-- 2. Every number is either greater than 5 or less than 6
-- A: forall n, (n > 5) v ((n > 6)
prob2 = and [(n > 5) || (n < 6) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall x, (forall y, x <= y || y <= x)
prob3 = and [and [y <= x || x <= y | y <- u] | x <- u]

-- 4. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forall x, (exists y, odd x -> (even y && y > x))
prob4 = and [odd x <= ( or [even y && y > x | y <- u])| x <- u]

-- 5. For every even number, there is a greater odd number
-- A: forall x, even x -> (exists y, (odd y && y > x))
prob5 = and [even x <= (or [odd y && y > x| y<-u])|x<-u]

-- 6. There are two odd numbers that add up to 6
-- A: Exists x, exists y, odd x ^ odd y ^ x + y == 6
prob6 = or [x + y == 6 | x <- u, y <- u]

-- 7. There are two even numbers that add up to 20
-- A: thereis n, (thereis m, (n + m = 6))
prob7 = or [(n + m == 20) | n <- (filter (\x -> even x) u), m <- (filter (\x -> even x) u) ]

-- 8. There is a largest number (i.e., there is a number that is >= all numbers)
-- A: Exists x, forall y, x >= y
prob8 = or [and [x >= y | y <-u ] | x <- u ]

-- 9. For every two different numbers, there is a third number in between.
-- A: 
prob9 = undefined

-- 10. For every number, there is a different number such that there are no
--     numbers between these two.
-- A:  forall z, exists y, forall x , not (((x<y) ^ (x > z)) v ((x < z) ^ (z > y)) )
prob10 = and[ or[ and[ not(((x<y) && (x > z)) || ((x < z) && (z > y)) ) | x<-u] | y<-u] | z<-u]

