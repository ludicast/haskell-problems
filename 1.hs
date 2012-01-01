import Data.List

-- 1) 
-- sum [x|x <- [1..1000], (x `mod` 3 == 0 or x `mod` 5 == 0) ]
-- 234168


-- 2)
fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

fibs = sum . filter even . takeWhile (<=4000000) . map fib $ [1..]
-- 4613732

-- 3)

isPrime x = 
	if x == 1 || x == 2 
		then
			True 
		else
			not $ any (\y -> x `mod` y == 0) [2..(x-1)]

genPrimes = filter isPrime [2..]

getDivisorList n = takeWhile (\x -> x < n ) genPrimes

divBy num divisor list = 
	if num `mod` divisor == 0 
		then 
			let newNum = num `div` divisor in
				divBy newNum divisor (divisor:list)
		else
			(num, list)
	
divisors n =
	case divisorList n of
		(1, list) -> list
		(n, []) -> [n]
	where
		divisorList n = foldl (\(value, list) prime -> divBy value prime list) (n, []) (getDivisorList n)

-- 4)

palindrome xs = reverse xs == xs

pals = (map (\x -> read x :: Int) . filter palindrome . map show) [ x * y | x <- [100..999], y <- [100..999]]

largestPal = last pals
-- 580085

-- 5)

divLists = map divisors [2..20]
compacted = sort . concat $ map group divLists

smallestDividedBy20 = product $ foldr1 (\nums acc -> if head nums == head acc then acc else nums ++ acc) compacted

-- 232792560

-- 6)


