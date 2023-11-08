module Newton where

{- Let f(x) is a function. Find x such that f(x) = 0
   Basic algorithm:
     Step 1: Take initial approximation 
     Step 2: If the approximation is good enough return it, otherwise improve it and repeat Step 2
-}

newton :: (t -> Bool) -> (t -> t) -> t -> t
newton isGood improve = start where
    start x | isGood x = x
            | otherwise = start $ improve x

-- Let find ⁿ√x. The function is f(x) = xⁿ - a and the task is to solve xⁿ - a = 0

root :: (Integral p, Num t, Ord t) => p -> (t -> t -> t) -> t -> t -> t
root n improveRoot eps a = newton isGoodRoot (improveRoot a) 1 where
    isGoodRoot x = abs (x ^ n - a) < eps

eps :: Double
eps = 0.00000000000000001

sqrt' :: Double -> Double
sqrt' = root 2 (\a x -> (x + a / x) / 2) eps

cbrt :: Double -> Double
cbrt = root 3 (\a x -> (2 * x + a / x ^ 2) / 3) eps
