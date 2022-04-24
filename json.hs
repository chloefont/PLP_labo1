import Data.Char
import System.Environment


testInput = "{ " ++ '"':"str" ++ '"' : ": [ 123, true, null ] }"

isQuot :: Char -> Bool
isQuot c = c == '"'

tokenize' [] = []
tokenize' (c:cs)
    | isSpace c = tokenize' cs
    | isQuot c = tokenize' cs
    | isAlpha c = onAlpha (c:cs)
    | isDigit c = onDigit (c:cs)
    | otherwise = [c] : tokenize' cs

onDigit cs = token:tokenize' rest
    where (token,rest) = span isDigit cs

onAlpha cs = token:tokenize' rest
    where (token,rest) = span isAlpha cs

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ tokenize' json
