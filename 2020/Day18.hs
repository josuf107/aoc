import Text.ParserCombinators.ReadP
import Data.Char

main = do
    input <- readFile "input18.txt"
    let parsedInput = lines . filter (/=' ') $ input
    print . sum . fmap (fst . head) . fmap (readP_to_S $ inputp <* eof) $ parsedInput
    print . sum . fmap (fst . head) . fmap (readP_to_S $ inputp2 <* eof) $ parsedInput

inputp = chainl1
    (choice [intp, parenp inputp])
    (choice [opp '+' (+), opp '*' (*)])

intp = read <$> many1 (satisfy isDigit)

inputp2 = chainl1
    addp
    (opp '*' (*))

addp = chainl1
    (choice [intp, parenp inputp2])
    (opp '+' (+))

opp c f = char c >> return f

parenp p = char '(' *> p <* char ')'
