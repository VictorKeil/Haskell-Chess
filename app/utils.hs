module Utils where

-- |Like takeWhile, but includes the first element for which the predicate is false
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = if f x then x:(takeUntil f xs) else [x]

-- |Takes a function and its argument, applies the function to the argument and stores
-- the result and the argument in a pair
fArgPair :: (a -> b) -> a -> (b, a)
fArgPair f x = (f x, x)

-- |Takes a list of Bools and one of any type, pairs up the elements of the lists,
-- and returns a filtered version of the second list containing every element that
-- was paired with a True value
zipFilter :: [Bool] -> [a] -> [a]
zipFilter [] _ = []
zipFilter _ [] = []
zipFilter (b:bs) (x:xs) = if b then x:(zipFilter bs xs) else zipFilter bs xs

-- |Cycle through a bounded, enumerated type
cycleEnumSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumSucc e = if e == maxBound then minBound else succ e

-- |Cycle through a bounded, enumerated type
cycleEnumPred :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumPred e = if e == minBound then maxBound else pred e

