-- CSci 60 Lab 4

---- Solution to Extended GCD (again) ----

-- usage: extGCD a b = (d, m, n)
-- assumes: a >= 0, b >= 0
-- insures: d = gcd(a,b), m*a + n*b = d
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (q, r) = divMod a b
                 (d, m, n) = extGCD b r
             in (d, n, m - n*q)


---- Part I:  Some helper functions ----

-- Coprime with a list
-- usage: coprime_with x ys = anss
-- assumes: nothing
-- insures: ans == True, if x is coprime with each element of ys
--          ans == False, otherwise
coprime_with :: Integer -> [Integer] -> Bool
coprime_with a [] = True
coprime_with a (b:bs) = gcd a b == 1 && coprime_with a bs

-- Pairwise Coprime
-- usage: pairwise_coprime xs = ans
-- assumes: nothing
-- insures: ans == True, if the elements of the list xs are pairwise coprime
--          ans == False, otherwise
pairwise_coprime :: [Integer] -> Bool
pairwise_coprime [] = True
pairwise_coprime (a:as) = coprime_with a as && pairwise_coprime as


-- Multiplicative inverse (using extGCD)
-- usage: minv a m = ainv
-- assumes: a >= 0, m >= 2, and gcd(a,m) = 1
-- insures: 0 <= ainv < m, a * ainv â‰¡ 1 (mod m)
minv :: Integer -> Integer -> Integer
minv a m = let (x,y,z) = extGCD a m
           in y `mod` m

---- Part II: Chinese Remainder Theorem and Exponential Mod ----

-- Does a given Chinese Remainder Theorem problem have a solution?
-- usage: crtHasSolution as ms = ans
-- insures: ans == True, if
--                  (1) the lists as and ms are the same length and nonempty,
--                  (2) each element mi of ms satisfies mi >= 2, and
--                  (3) the elements of ms are pairwise coprime;
--          ans == False, otherwise.
crtHasSolution :: [Integer] -> [Integer] -> Bool
crtHasSolution as ms = length as == length ms 
                                    && all ( >= 2 ) ms 
                                    && pairwise_coprime ms 
                                    && all ( >=0 ) as 

-- Is a given number a solution to a CRT problem?
-- usage: crtIsSolution n as ms = ans
-- insures: ans == True, if crtHasSolution as ms == True and n is a solution
--          ans == False, otherwise
crtIsSolution :: Integer -> [Integer] -> [Integer] -> Bool
crtIsSolution n as ms = crtHasSolution as ms 
                        && map (mod n) as == ms

-- Chinese Remaninder Theorem
-- usage: crt as ms = ans
-- assumes: nothing
-- insures: ans == Nothing, if crtHasSolution as ms == False
--          ans == Just n, if n is such that crtIsSolution n as ms == True
--                         and 0 <= n < product ms                 
crt :: [Integer] -> [Integer] -> Maybe Integer
crt as ms = let
                pro = product ms
                mis = map (div pro) ms
                minvs = zipWith (minv) mis ms
                p = zipWith3 (\a b c -> a * b * c) as mis minvs
            in Just ((sum p)`mod` pro)


-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- insures: a^n â‰¡ b (mod m), 0 <= b < m
expmod :: Integer -> Integer -> Integer -> Integer
expmod a n m 
             | n == 0 = 1
             | n == 1 = a `mod` m
             | even n = let q = (expmod a (n `div` 2) m) `mod` m in (q^2) `mod` m
             | otherwise = (a * expmod a (n-1) m) `mod` m 
--ex.) expmod 3 2 12 = 9 
