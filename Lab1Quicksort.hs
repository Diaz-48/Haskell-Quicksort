-- Anthony Diaz
-- 014058185
-- Due: 2/11/2020
-- Dr. Jurgensen
-- CECS 424-03
module Quick where

qsort :: Ord a => [a] -> [a]	-- quick sort a list in Haskell
qsort a 						-- Quicksorts a list given a list a
	| (length a) <= 1	= a 	-- If the length of a is less than or equal to 1, then return the list.
	| otherwise 		= qsort small ++ (pivot : qsort big)	-- Recursion call on small, add with the pivot, then recursion call on big.
		where													-- Where
			pivot = head a 										-- Make the pivot the head of the list.
			tail = drop 1 a 									-- Make the tail the rest of the list after the head.
			small = [x | x <- tail, x <= pivot]					-- Make small the elements in the tail that are less than or equal to the pivot.
			big = [x | x <- tail, x > pivot]					-- Make big the elements in the tail that are more than the pivot.

main :: IO()												-- Main function.
main = print(qsort [4, 65, 2, -31, 0, 99, 2, 83, 782, 1])	-- Prints list after being quick sorted.