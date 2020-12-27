import Text.ParserCombinators.ReadP
import Data.Char
import Data.Bits
import qualified Data.Map.Strict as Map

data Line = Mask String | Mem Int Int deriving (Show, Eq, Ord)

main = do
    input <- readFile "input14.txt"
    let parsedInput = fmap (fst . head . readP_to_S linep) .  lines $ input
    let (memory, _) = foldr (\line (memory, mask) -> stepProgram memory mask line) (Map.empty, "") . reverse $ parsedInput
    print . sum $ memory
    let (memory, _) = foldr (\line (memory, mask) -> stepProgram' memory mask line) (Map.empty, "") . reverse $ parsedInput
    print . sum $ memory
    putStrLn "done."

stepProgram memory mask line = case line of
    Mask mask' -> (memory, mask')
    Mem address value -> (Map.insert address (applyMask mask value) memory, mask)

stepProgram' memory mask line = case line of
    Mask mask' -> (memory, mask')
    Mem address value -> (foldr (\address' memory' -> Map.insert address' value memory') memory (applyMask' mask address), mask)

linep = choice [Mask <$> (string "mask = " *> many1 get), Mem <$> (string "mem[" *> intp) <*> (string "] = " *> intp)] <* eof

intp = read <$> many1 (satisfy isDigit)

applyMask mask n = foldr (\(bit, v) n' -> fixBit bit v n') n . zip [0..] . reverse $ mask
    where
        fixBit bit v n' = case v of
            '1' -> setBit n' bit
            '0' -> clearBit n' bit
            _ -> n'

applyMask' mask n = foldr (\(bit, v) ns -> fixBit bit v ns) [n] . zip [0..] . reverse $ mask
    where
        fixBit bit v ns = case v of
            '1' -> fmap (\n' -> setBit n' bit) ns
            '0' -> ns
            'X' -> concat . fmap (\n' -> [setBit n' bit, clearBit n' bit]) $ ns
            x -> error $ "What is " ++ [x] ++ "?"

-- 10931980247994 is too low
-- 11327140210986
