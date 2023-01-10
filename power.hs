module Power where
-- Code for the Power to the People Coursework.
-- Write all your code in this file.

---------------------------------------------
-- An example implementation of the power function.
-- *DO NOT EDIT THIS FUNCTION*

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- Task 1 -------------------------

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- Task 2 -------------------------

power1 :: Integer -> Integer -> Integer
power1 n k = product (replicate (fromInteger k) n)

-- Task 3 -------------------------

power2 :: Integer -> Integer -> Integer
power2 n k 
   | k == 0          = 1
   | k > 0 && even k = (n*n) ^ (k `div` 2)
   | k > 0 && odd k  = n * (n ^ (k-1))

-- Task 4 -------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k 
   | power n k == power1 n k = True
   | otherwise               = False

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k 
   | power n k == power2 n k = True
   | otherwise               = False

-- Task 5 -------------------------

-- Each entry should be in this format: (n, k, result of comparePower1, result of comparePower2)
comparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
comparisonList ns ks = [ (w, x, y, z) | w <- ns , x <- ks , y <- [comparePower1 w x], z <- [comparePower2 w x] ]