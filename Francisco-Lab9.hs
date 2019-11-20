---- Lab 9 ----

-- Sets (of Ints) as lists
--
-- Sets are collections of elements for which order and duplicates don't matter.
-- So, the set {5,3,2,3,5,2} is the same as the set {2,3,5}. All that matters
-- is which elements are members, in this case 2, 3, and 5 (in no particular
-- order). In Haskell, we can represent sets as lists, although we then have
-- to be careful not to care about the order or introduce duplicates when we
-- compute with them.

type Set = [Int]

-- Implement the following set functions as LIST RECURSIONS. You can ASSUME
-- that the inputs do not have any duplicates, and must INSURE that the outputs
-- also do not contain duplicates.

-- element x xs = True if x is an element in the set xs
-- Examples: element 3 [5,2,3,6] == True, element 1 [5,2,3,6] == False
element :: Int -> Set -> Bool
element n [] = False
element n (x:xs) = if n == x
                   then True
                   else element n xs

-- subset xs ys = True if every element of xs is also an element of ys
-- Examples: subset [2,6,3] [5,2,3,6] == True, subset [2,7] [5,2,3,6] == False
subset :: Set -> Set -> Bool
subset xs ys = all (`element` ys) xs

-- cardinal xs = n if the number of elements in xs is n
-- Examples: cardinal [5,2,3,6] = 4, cardinal [2,7] = 2
cardinal :: Set -> Int
cardinal = cardinal 0
    where cardinal a [] = a
          cardinal a (_:xs) = cardinal (a+1) xs

-- inters xs ys = zs if zs is the INTERSECTION of xs and ys, i.e., all the
-- elements the sets xs and ys have in common.
-- Example: inters [5,2,3,6] [3,7,4,2] = [2,3]
inters :: Set -> Set -> Set
inters xs ys = let ns = [ a | a <- xs, element a ys] in [ b | b <- ys, element b ns]

-- union xs ys = zs if zs is the UNION of xs and ys, i.e., all the
-- elements that appear in either of the sets xs and ys.
-- Example: union [5,2,3,6] [3,7,4,2] = [5,6,3,7,4,2]
union :: Set -> Set -> Set
union xs ys = foldl (\as y -> if element y as then as else as ++ [y]) xs ys

-- differ xs ys = zs is zs is the DIFFERENCE of xs and ys, i.e., all the
-- elements that appear in xs but do not appear in ys.
-- Example: differ [5,2,3,6] [3,7,4,2] = [5,6]
differ :: Set -> Set -> Set
differ xs ys = undefined

-- Although sets do not contain duplicates, ordinary lists can, so it may
-- be useful to have functions to detect and remove duplicates.

-- is_set xs = True if xs does not have any duplicates
-- Examples: is_set [5,3,2,3,5,2] == False, is_set [3,5,2] == True
is_set :: Set -> Bool
is_set [] = False
is_set [_] = False
is_set (x:s) = if element x s then False
                              else True

-- rem_dups xs = ys if ys is the list xs with the duplicates removed
-- Example: rem_dups [5,3,2,3,5,2] = [3,5,2]
rem_dups :: Set -> Set
rem_dups [] = []
rem_dups (x:xs)  | x `elem` xs   = rem_dups xs
                 | otherwise     = x : rem_dups xs

--Other Way to do it 
--rem_dups [x] = [x]
--rem_dups (x:xs) = x : [ k  | k <- rem_dups(xs), k /=x ]