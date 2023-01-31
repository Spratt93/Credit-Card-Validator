-- ****EX1****

--toDigits 1234 returns [1,2,3,4]
toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n
 | n >=0 = toDigits(n `div` 10) ++ [n `mod` 10]
 | otherwise = []

--toDigitsRev 1234 returns [4,3,2,1]
toDigitsRev :: Int -> [Int]
toDigitsRev 0 = []
toDigitsRev n
 | n >= 0 = n `mod` 10 : toDigitsRev(n `div` 10) 
 | otherwise = []

-- ****EX2****

--doubles every other no. beginning from the rhs of list
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:y:z)
 | (length (x:y:z)) `mod` 2 /= 0 = x : y*2 : doubleEveryOther z
 | otherwise = x*2 : y : doubleEveryOther z

-- ****EX3****

-- calculates the sum of all digits
-- e.g [16,7,12,5] = 1+6+7+1+2+5==22
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs)
 | x < 10 = x + sumDigits xs
 | otherwise = (x `mod` 10) + (x `div` 10) + sumDigits xs 


-- ****EX4****
--function confirms if the number is a valid credit card number
--number is valid if remainder when div by 10 is 0
validate :: Int -> Bool
validate n
 | (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
 | otherwise = False

-- main function
main = do
   putStrLn("Enter your credit card number")
   a <- (readLn)
   let b = validate a
   if (b == True)
       then putStrLn("Valid")
       else putStrLn("Invalid")
