import Data.List

-- 1) 
-- sum [x|x <- [1..999], (x `mod` 3 == 0 or x `mod` 5 == 0) ]
-- 233168


-- 2)
fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

fibs = sum . filter even . takeWhile (<=4000000) . map fib $ [1..]
-- 4613732

-- 3)


primes (x:xs) =
	x:primes (filter indivisible xs)
		where
			indivisible n = n `mod` x /= 0

genPrimes = primes [2..]

getDivisorList n = takeWhile (\x -> x < n ) genPrimes

divBy num divisor list = 
	if num `mod` divisor == 0 
		then 
			let newNum = num `div` divisor in
				divBy newNum divisor (divisor:list)
		else
			(num, list)
	
divisors n =
	getDivisors n genPrimes
		where
			getDivisors 1 _ = []
			getDivisors n primeList@(x:xs) | n `mod` x == 0 =
				x : getDivisors (n `div` x) primeList
			getDivisors n (x:xs) = getDivisors n xs

-- 4)

palindrome xs = reverse xs == xs

pals = (map (\x -> read x :: Int) . filter palindrome . map show . sort) [ x * y | x <- [100..999], y <- [100..999]]

largestPal = last pals
-- 906609

-- 5)

divLists = map divisors [2..20]
compacted = sort . concat $ map group divLists

smallestDividedBy20 = product $ foldr1 (\nums acc -> if head nums == head acc then acc else nums ++ acc) compacted

-- 232792560

-- 6)

sumOfSquares n = sum $ (map (^2)) [1..n]
squareOfSums n = sum [1..n] * sum [1..n]

-- squareOfSums 100 - sumOfSquares 100
-- 25164150

-- 7)

-- last $ take 10001 $ genPrimes             104743
--

-- 8)

bigNum = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

bigNumChars = show bigNum

splitInFives = map (\i -> take 5 $ drop i bigNumChars)

fiveBys =  (map (\x -> map (\char -> read [char]::Int) x) .  splitInFives) [0..995]

biggest = last . sort $ map product fiveBys
-- 40824

-- 9) 
(tripleta, tripletb, tripletc) = head [(a,b,c) | a <- [1..1000], b <- [(a + 1)..1000], c <- [(b + 1)..1000], a + b + c == 1000, a^2 + b^2 == c^2]

triplet = tripleta * tripletb * tripletc
-- 31875000

-- 10)
first2mil = takeWhile (<2000000) genPrimes
tots = sum first2mil

-- 142913828922

-- 11)
nums = (map (map (\n -> read n :: Int)) . map words) ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08","49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00","81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65","52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91","22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80","24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50","32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70","67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21","24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72","21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95","78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92","16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57","86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58","19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40","04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66","88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69","04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36","20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16","20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54","01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]

diagonalMap directionName rule opp startRow startColumn =
	if startRow < 17 && (rule startColumn)
		then 
			[(directionName, [ (nums !! (startRow + step)) !! (startColumn `opp` step) | step <- [0..3]])]
		else []

diagonalMaps startRow startColumn = 
	diagonalMap "diagonalLeft" (>2) (-) startRow startColumn ++
	diagonalMap "diagonalRight" (<17) (+) startRow startColumn 

horizontalMap startRow startColumn = 
	if startColumn < 17
		then
			[("horizontal", [(nums !! startRow) !! column | column <- [startColumn..startColumn + 3]])]
		else
			[]

verticalMap startRow startColumn = 
	if startRow < 17
		then
			[("vertical", [(nums !! row) !! startColumn | row <- [startRow..startRow + 3]])]
		else
			[]

mapDirs row column =
	map computeProducts $ horizontalMap row column ++ verticalMap row column  ++ diagonalMaps row column
	where 
		computeProducts (typeName, values) = (typeName, values, product values)
		

mappedNums' = filter notEmptyDirections $ [ ((column, row), mapDirs row column) | row <- [0..19], column <- [0..19] ]
	where 
		notEmptyDirections (_, xs) = (not . null) xs

mappedNums = map throwOutInferior mappedNums'
	where
		throwOutInferior (location, xs) = (location, maximumBy biggestProduct xs)
		biggestProduct (_, _, product1) (_, _, product2) = product1 `compare` product2

biggestOf400 = maximumBy productVal mappedNums
	where
		productVal (_, (_,_,product1)) (_, (_,_,product2)) = product1 `compare` product2
-- ((6,12),("diagonalLeft",[89,94,97,87],70600674))

-- 12)
--
--

triangleNumber n | odd n =
	((n + 1) `div` 2) * n
triangleNumber n | even n =
	(n + 1)  * (n `div` 2)

-- isTriangle n = isTriangleForEven n || isTriangleForOdd n

triangleNumbers = map triangleNumber [1..]

-- triangleNumbersWithDivisors min = (\n -> (n, divisors n)) $ head . filter (\n -> length (divisors n) > min) $ triangleNumbers


comboList :: Int -> [Integer] -> [[Integer]]

comboList remaining (x:[]) = [ replicate remaining x ]
comboList remaining (x:xs) = 
	concat [ map (\list -> (replicate count x) ++ list) (comboList (remaining - count) xs) | count <- [0..remaining]]

cleanComboList :: Int -> [Integer] -> [[Integer]]
cleanComboList remaining list =
	let optionList = comboList remaining list in
		filter (\options -> (last list) `elem` options) optionList

isTriangleNum n = (==) n $ last $ takeWhile (\x -> x <= n) triangleNumbers

mapLists :: Int -> [Integer] -> [Integer] -> [[Integer]]
mapLists n (x:xs) cached =
	let subList = cached ++ [x] in
		cleanComboList n subList ++ mapLists n xs subList

take 150  $ mapLists 20  genPrimes []
head . filter (\list -> (isTriangleNum . product) list) $ mapLists 20  genPrimes []

-- too slow to use...
bboy = head . map (\l -> (l, product l)) . filter (\list -> (isTriangleNum . product) list) $ mapLists 501  genPrimes []
