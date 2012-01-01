myLast = head . reverse
myLast' = foldr1 (\x y -> y)
myLast'' = foldr1 (flip const)

myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs


elementAtRev 1 = head
elementAtRev n = elementAtRev (n - 1) . tail
elementAt = flip elementAtRev


elementAt' xs n = xs !! (n - 1)

myLength [] = 0
myLength xs = 1 + (myLength . tail) xs


myRev [] = []
myRev (x:xs) =  myRev xs ++ [x]

myRev' = foldl (\acc x -> x:acc) []

isPalindrome xs = xs == reverse xs
