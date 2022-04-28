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




isSpecial c = elem c ":"

data Token = JKeyValue (Token, Token) | JObject [Token] | JNumber Int | JString String | JBool Bool | JNull | JArray [Token] | JSpecial String | JError String
    deriving (Show)
lexer :: String -> [Token]
lexer cs =
    let
        f:: [Token] -> [Token]
        f [] = []
        f ((JObject a):xs) = JObject (f a): (f xs) 
        f ((JArray a):xs) = JObject (f a): (f xs) 
        f (a:(JSpecial _):c:xs) = (JKeyValue (a,c)):(f xs)
        f (a:xs) = a:(f xs)
        value = f (fst (lexer' cs))
    in
        value

lexer' :: String -> ([Token], String)
lexer' [] = ([], "")
lexer' (c:cs)
    | isSpace c = (fst (lexer' cs), "")
    | ',' == c = (fst (lexer' cs), "")
    | '[' == c = (onArray cs, "")
    | '"' == c = (onString cs, "")
    | '\'' == c = (onString2 cs,"")
    | '{' == c = (onObject cs, "")
    | isSpecial c = (token JSpecial isSpecial (c:cs), "")
    | isAlpha c = (onAlpha (c:cs),"")
    | isDigit c = (onDigit (c:cs), "")
    | '}' == c = ([], (cs))
    | ']' == c = ([], (cs))
    | otherwise = (JError(show (c:cs)) : [], "")

token constructor filter cs = constructor token: fst (lexer' rest)
    where (token, rest) = span filter cs

onString2 cs = JString (token) : fst (lexer' rest)
    where (token, (r:rest)) = span (/= '\'') cs

onString cs = JString (token) : fst (lexer' rest)
    where (token, (r:rest)) = span (/= '"') cs

onArray cs = 
    let 
        (value, other) = lexer' cs
    in
        JArray (value) : fst (lexer' other)

onObject cs = 
    let
        (value, other) = lexer' cs
    in
        JObject (value) : fst (lexer' other)
        

onDigit cs = JNumber (read token :: Int): fst (lexer' rest)
    where (token, rest) = span isDigit cs

onAlpha cs =
    let
        onAlpha' os
            | os == "true" = JBool True
            | os == "false" = JBool False
            | os == "null" = JNull
            | otherwise = JError os
    in
        onAlpha' token :  fst (lexer' rest)
    where (token, rest) = span isAlpha cs
    

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ lexer json
