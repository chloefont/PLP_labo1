import Data.Char
import System.Environment
import Control.Exception


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


data JsonException = WrongFileFormat String
    deriving (Show)
instance Exception JsonException


data Token = JKeyValue (Token, Token) | JObject [Token] | JArray [Token] | JNumber Int | JString String | JBool Bool | JNull | JSpecial String | JError String


instance Show Token where
    show (JNumber x) = show x
    show (JString s) = show s
    show (JBool b) = show b
    show JNull = show "null"
    show (JSpecial s) = show s
    show (JKeyValue (key, value)) = show key ++ ": " ++ show value
    show (JObject ts) = (foldl (\acc x -> acc ++ (show x) ++ ", ") "{" ts) ++ "}"
    show (JArray ts) = show ts

lexer :: String -> [Token]
lexer cs =
    let
        -- permet de créer les clés valeurs (JKeyValue)
        f:: [Token] -> [Token]
        f [] = []
        f ((JObject a):xs) = JObject (f a): (f xs) 
        f ((JArray a):xs) = JObject (f a): (f xs) 
        f (a:(JSpecial _):(JObject c):xs) = (JKeyValue (a,JObject(f c))):(f xs)
        f (a:(JSpecial _):(JArray c):xs) = (JKeyValue (a,JArray(f c))):(f xs)
        f (a:(JSpecial _):c:xs) = (JKeyValue (a,c)):(f xs)
        f (a:xs) = a:(f xs)
    in
        f (lexer' cs)

-- Analyse lexical d'un json
lexer' :: String -> [Token]
lexer' [] = []
lexer' (c:cs)
    | isSpace c = (lexer' cs)
    | ',' == c = (lexer' cs)
    | '[' == c = (arrayToken JArray (/=']') cs)
    | '"' == c = (token JString (/='"') cs)
    | '\'' == c = (token JString (/='\'') cs)
    | '{' == c = (arrayToken JObject (/='}') cs)
    | ':' == c = (JSpecial ":") : lexer' cs 
    | isAlpha c = (onAlpha (c:cs))
    | isDigit c = (onDigit (c:cs))
    | '}' == c = []
    | ']' == c = []
    | otherwise = throw (WrongFileFormat ("on: " ++ [c]))


token constructor filter cs = constructor (token) : (lexer' rest)
    where (token, (r:rest)) = span filter cs

-- Construction des objets ou tableau
arrayToken constructor filter cs = 
    let 
        (_, (_:rest)) = span filter cs
        value = lexer' cs
    in
        (constructor value) : (lexer' rest)

-- Conversion des nombres en Int
onDigit cs = JNumber (read token :: Int): (lexer' rest)
    where (token, rest) = span isDigit cs

-- Check des bool et null
onAlpha cs =
    let
        onAlpha' os
            | os == "true" = JBool True
            | os == "false" = JBool False
            | os == "null" = JNull
            | otherwise = throw (WrongFileFormat ("on: " ++ os))
    in
        onAlpha' token :  (lexer' rest)
    where (token, rest) = span isAlpha cs

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ lexer json
