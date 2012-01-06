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

sumOfSquares n = sum $ (map (^2)) [1..n]
squareOfSums n = sum [1..n] * sum [1..n]

diffFirst100 = map (\n -> squareOfSums n - sumOfSquares n) [1..100]

-- 0,4,22,70,170,350,644,1092,1740,2640,3850,5434,7462,10010,13160,17000,21624,27132,33630,41230,50050,60214,71852,85100,100100,117000,135954,157122,180670,206770,235600,267344,302192,340340,381990,427350,476634,530062,587860,650260,717500,789824,867482,950730,1039830,1135050,1236664,1344952,1460200,1582700,1712750,1850654,1996722,2151270,2314620,2487100,2669044,2860792,3062690,3275090,3498350,3732834,3978912,4236960,4507360,4790500,5086774,5396582,5720330,6058430,6411300,6779364,7163052,7562800,7979050,8412250,8862854,9331322,9818120,10323720,10848600,11393244,11958142,12543790,13150690,13779350,14430284,15104012,15801060,16521960,17267250,18037474,18833182,19654930,20503280,21378800,22282064,23213652,24174150,25164150]

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
triangleNumbers = map (\n -> sum [1..n]) [1..]
triangleNumbersWithDivisors = map (\n -> (n, divisors n)) triangleNumbers


