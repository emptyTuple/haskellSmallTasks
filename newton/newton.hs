module Newton where

{- Let f(x) is a function. Find x such that f(x) = 0
   Basic algorithm:
     Step 1: Take initial approximation 
     Step 2: If the approximation is good enough return it, otherwise improve it and repeat Step 2
-}

newton :: (t -> Bool) -> (t -> t) -> t -> t
newton isGood improve = start where
    start x | isGood x = x
            | otherwise = start (improve x)

