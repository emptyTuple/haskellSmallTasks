module Fibbonacci where

-- | Naive implementation O(eⁿ)
fibbonacciNaive :: Int -> Integer
fibbonacciNaive n | n == 0    = 0
                  | n == 1    = 1
                  | otherwise = fibbonacciNaive (n - 1) + fibbonacciNaive (n - 2)

-- | Tail recursive function
fibbonacciHelper :: (Eq a, Num a) => a -> a
fibbonacciHelper n = helper 0 1 n where
  helper x y 0 = x
  helper x y n = helper y (x + y) (n - 1)

fibbonacciToList :: Num a => Int -> [a]
fibbonacciToList n = helper n 0 1 where
  helper 0 _ _ = []
  helper n x y = x : helper (n - 1) y (x + y)

-- | infinite flow of fibonacci numbers
fibbonacciInfinite :: Num a => [a]
fibbonacciInfinite = helper 0 1 where
  helper :: Num a => a -> a -> [a]
  helper x y = x : helper y (x + y)

takeNfibbonacci :: Num a => Int -> [a]
takeNfibbonacci n = take n fibbonacciInfinite
-- ^ take n numbers of flow

--  O(log n) implementation

{- A generalized O(log n) power finction implementation following the next algorithm:
    x²ⁿ = (x²)ⁿ
    x²ⁿ⁺¹ = x·(x²)ⁿ
-}
powGen :: Integral a => (t -> t -> t) -> a -> t -> t -> t
powGen f n x c
       | n == 0    = c
       | even n    = powGen f (n `div` 2) (x `f` x) c
       | otherwise = powGen f (n - 1) x (c `f` x)

{- Can see that:
    (0 1)  *  (F n - 1)  =  (F n    )
    (1 1)     (F n    )     (F n + 1)
  or
    (0 1)ⁿ  =  (F n - 1   F n    )
    (1 1)      (F n       F n + 1)

-}

-- | 2x2 matrix multiplication implementation
m2x2Mul :: Num a => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
m2x2Mul (x11, x12, x21, x22) (y11, y12, y21, y22) = (
  x11 * y11 + x12 * y21,   x11 * y12 + x12 * y22,
  x21 * y11 + x22 * y12,   x21 * y12 + x22 * y22
  )

-- | Use the generalized power function with the matrix multiplication function
fibbonacciOlogN :: Integral a => a -> a
fibbonacciOlogN n = x where
  mx2xPow m z = powGen m2x2Mul m z (1, 0, 0, 1)
  (_, x, _, _) = mx2xPow n (0, 1, 1, 1)
