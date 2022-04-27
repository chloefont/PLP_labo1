import Data.Char
import System.Environment


-- testInput = "{ " ++ '"':"str" ++ '"' : ": [ 123, true, null ] }"

-- isQuot :: Char -> Bool
-- isQuot c = c == '"'

-- tokenize' [] = []
-- tokenize' (c:cs)
--     | isSpace c = tokenize' cs
--     | isQuot c = tokenize' cs
--     | isAlpha c = onAlpha (c:cs)
--     | isDigit c = onDigit (c:cs)
--     | otherwise = [c] : tokenize' cs

-- onDigit cs = token:tokenize' rest
--     where (token,rest) = span isDigit cs

-- onAlpha cs = token:tokenize' rest
--     where (token,rest) = span isAlpha cs




isSpecial c = elem c "':,"

data Token = JObject [Token] | JNumber Int | JString String | JBool Bool | JNull | JArray [Token] | JSpecial String | JError String
    deriving (Show)
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | '[' == c = onArray cs
    | '"' == c = onString cs
    | '\'' == c = onString2 cs
    | '{' == c = onObject cs
    | isSpecial c = token JSpecial isSpecial (c:cs)
    | isAlpha c = onAlpha (c:cs)
    | isDigit c = onDigit (c:cs)
    | otherwise = JError(show (c:cs)) : []

token constructor filter cs = constructor token:lexer rest
    where (token, rest) = span filter cs

onString2 cs = JString (token) : lexer rest
    where (token, (r:rest)) = span (/= '\'') cs

onString cs = JString (token) : lexer rest
    where (token, (r:rest)) = span (/= '"') cs

onArray cs = JArray (lexer token) : lexer rest
    where (token, (r:rest)) = span (/=']') cs

onObject cs = JObject (lexer token) : lexer rest
    where (token, (r:rest)) = span (/='}') cs
    
onDigit cs = JNumber (read token :: Int):lexer rest
    where (token, rest) = span isDigit cs

onAlpha cs =
    let
        onAlpha' os
            | os == "true" = JBool True
            | os == "false" = JBool False
            | os == "null" = JNull
    in
        onAlpha' token : lexer rest
    where (token, rest) = span isAlpha cs
    

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ lexer json
