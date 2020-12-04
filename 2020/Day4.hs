import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char
import Debug.Trace

main = do
    input <- readFile "input4.txt"
    let parsedInput = fst . head . readP_to_S inputp $ input
    let validPassports = filter ((requiredFields `Set.isSubsetOf`) . Map.keysSet) $ parsedInput
    print . length $ validPassports
    let validPassports2 = filter validate validPassports
    print . length $ validPassports2

requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validate passport = all runRule . Map.toList $ passport
    where runRule (key, value) = not . null . readP_to_S (rules Map.! key >> eof) $ value

rules = Map.fromList
    [ ("byr", rangedNump 1920 2002)
    , ("iyr", rangedNump 2010 2020)
    , ("eyr", rangedNump 2020 2030)
    , ("hgt", choice [rangedNump 150 193 >> string "cm", rangedNump 59 76 >> string "in"])
    , ("hcl", char '#' >> count 6 (satisfy (`elem` "0123456789abcdef")))
    , ("ecl", choice (fmap string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
    , ("pid", count 9 (satisfy isDigit))
    , ("cid", many1 get)
    ]

rangedNump min max = do
    num <- (read :: String -> Int) <$> many1 (satisfy isDigit)
    if num >= min && num <= max then return (show num) else pfail

inputp = do
    passportp `sepBy` (string "\n\n") <* (char '\n' >> eof)
    where
        passportp = Map.fromList <$> (tuplep `sepBy` (satisfy (`elem` " \n")))
        tuplep = (,) <$> (many1 notSpecial <* char ':') <*> (many1 notSpecial)
        notSpecial = satisfy (`notElem` " \n:")
