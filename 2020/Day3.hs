import qualified Data.Map.Strict as Map
import Data.Maybe

main = do
    input <- readFile "input3.txt"
    let grid = readGrid input
    let rows = (+1) . maximum . fmap fst . Map.keys $ grid
    let columns = (+1) . maximum . fmap snd . Map.keys $ grid
    let trees = fmap (\(down, right) -> checkSlope down right grid columns) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    print . product $ trees

checkSlope down right grid columns = 
    let
        points = iterate (\(row, column) -> (row + down, column + right)) (0, 0)
        values = catMaybes . takeWhile isJust . fmap (\(row, column) -> getLetter grid columns row column) $ points
        trees = length . filter (=='#') $ values
    in trees

readGrid gridString = Map.fromList . indexGrid $ gridString
    where
        indexGrid = concat . fmap indexLine . indexList . lines
        indexList = zip [0..]
        indexLine (row, line) = fmap (indexLetters row) . indexList $ line
        indexLetters row (column, letter) = ((row, column), letter)

getLetter :: Map.Map (Int, Int) Char -> Int -> Int -> Int -> Maybe Char
getLetter grid columns row column = Map.lookup (row, column `mod` columns) grid
