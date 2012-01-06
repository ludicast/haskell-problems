import Sudoku
import System.IO

main = do
	handle <- openFile "sample_sudoku" ReadMode	
	contents <- hGetContents handle
	putStr . showSolution $ contents
	hClose handle
