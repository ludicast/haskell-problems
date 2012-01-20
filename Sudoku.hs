module Sudoku
( SudokuPuzzle(..),
	fromStringRows,
	fromStringRow,
	toStringRow,
	showSolution
) where

data SudokuPuzzle = SudokuPuzzle [[Maybe Int]] 

instance Show SudokuPuzzle where
	show (SudokuPuzzle rows) = unlines . map show .  map toStringRow $ rows

toStringRow :: [Maybe Int] -> String
toStringRow = unwords . map maybeToInt
	where 
		maybeToInt Nothing = "0"
		maybeToInt (Just n) = show n

showSolution :: String -> String
showSolution = show . solve . fromStringRows

solve :: SudokuPuzzle -> SudokuPuzzle
solve = id

fromStringRow :: String -> [Maybe Int]
fromStringRow = map intToMaybe . map (\char -> read char :: Int) . words
	where
		intToMaybe x = if x == 0 
			then Nothing
			else Just x

fromStringRows :: String -> SudokuPuzzle
fromStringRows rows = 
	let x = map fromStringRow . lines $ rows
		in SudokuPuzzle x


