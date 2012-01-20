import Sudoku
import System.IO
import System.Environment

getFileName :: IO String
getFileName = do
	xs <- getArgs
	let fileName = if length xs > 0 then xs !! 0 else "sample_sudoku"
	return fileName

main = do
	xs <- getArgs
	if null xs
		then do
			putStrLn "You entered no arguments"
		else do
			putStrLn ("You entered " ++ show xs)

	fileName <- getFileName
	withFile fileName ReadMode(\handle -> do
		contents <- hGetContents handle
		putStr . showSolution $ contents)
