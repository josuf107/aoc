import Text.ParserCombinators.ReadP
import Data.Char

data Rule = Rule Int Int Char deriving (Show, Eq, Ord)
data InputLine = InputLine Rule String deriving (Show, Eq, Ord)

main = do
    input <- readFile "input2.txt"
    let inputLines = fst . head . readP_to_S inputP $ input
    print . length . filter validPassword $ inputLines
    print . length . filter validPassword2 $ inputLines

validPassword :: InputLine -> Bool
validPassword (InputLine (Rule lower upper letter) password) = (\x -> x >= lower && x <= upper) . length . filter (==letter) $ password

validPassword2 :: InputLine -> Bool
validPassword2 (InputLine (Rule lower upper letter) password) = (==1) . length . filter (\(i, l) -> l == letter && i `elem` [lower, upper]) . zip [1..] $ password

inputP :: ReadP [InputLine]
inputP = do
    inputs <- many lineP
    eof
    return inputs

lineP :: ReadP InputLine
lineP = do
    rule <- ruleP
    string ": "
    password <- get `manyTill` (char '\n')
    return $ InputLine rule password

ruleP :: ReadP Rule
ruleP = do
    lowerBound <- numP
    char '-'
    upperBound <- numP
    char ' '
    letter <- get
    return $ Rule lowerBound upperBound letter

numP :: ReadP Int
numP = read <$> many1 (satisfy isDigit)
