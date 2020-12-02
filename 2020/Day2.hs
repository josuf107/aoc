import Text.ParserCombinators.ReadP
import Data.Char

data InputLine = InputLine Int Int Char String deriving (Show, Eq, Ord)

main = do
    input <- readFile "input2.txt"
    let inputLines = fmap (fst . head . readP_to_S inputP') . lines $ input
    print . length . filter validPassword $ inputLines
    print . length . filter validPassword2 $ inputLines

validPassword :: InputLine -> Bool
validPassword (InputLine lower upper letter password) =
    (\x -> x >= lower && x <= upper) . length . filter (==letter) $ password

validPassword2 :: InputLine -> Bool
validPassword2 (InputLine lower upper letter password) =
    (==1) . length . filter (\(i, l) -> l == letter && i `elem` [lower, upper]) . zip [1..] $ password

inputP' :: ReadP InputLine
inputP' = InputLine
    <$> numP
    <*> (char '-' *> numP)
    <*> (char ' ' *> get)
    <*> (string ": " *> (get `manyTill` eof))

numP :: ReadP Int
numP = read <$> many1 (satisfy isDigit)
