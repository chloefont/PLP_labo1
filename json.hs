{-
Labo 1 - Exercice 2.4
Auteurs : Luca Coduri & Chloé Fontaine
-}

import Data.Char
import Data.List
import System.Environment
import Control.Exception


newtype JsonException = WrongFileFormat String
    deriving (Show)
instance Exception JsonException


data Token = JKeyValue (Token, Token) | JObject [Token] | JArray [Token] | JNumber Int | JString String | JBool Bool | JNull | JSpecial String


instance Show Token where
    show (JNumber x) = show x
    show (JString s) = show s
    show (JBool b) = show b
    show JNull = "null"
    show (JSpecial s) = show s
    show (JKeyValue (key, value)) = show key ++ ": " ++ show value
    show (JObject ts) = "{" ++ intercalate ", " (map show ts) ++ "}"
    show (JArray ts) = show ts

{-
    Analyse lexical d'un json
    Arguments:
        1. correspond au json à analyser
    Retourne soit un JArray soit un JObject
-}
lexer :: String -> Token
lexer cs =
    let
        -- permet de créer les clés valeurs (JKeyValue)
        f:: [Token] -> [Token]
        f [] = []
        f ((JObject a):xs) = JObject (f a): f xs
        f ((JArray a):xs) = JArray (f a): f xs
        f (a:(JSpecial _):(JObject c):xs) = JKeyValue (a,JObject(f c)):f xs
        f (a:(JSpecial _):(JArray c):xs) = JKeyValue (a,JArray(f c)):f xs
        f (a:(JSpecial _):c:xs) = JKeyValue (a,c):f xs
        f (a:xs) = a:f xs
        -- retirer le premier et dernier caractère ({} ou [])
        (c:ns) = init cs
    in
        if c == '{' then JObject (f (lexer' ns)) else JArray (f (lexer' ns))

{-
    Analyse lexical d'un json
    Arguments:
        1. correspond au json à analyser
    Retourne une liste de tokens
-}
lexer' :: String -> [Token]
lexer' [] = []
lexer' (c:cs)
    | isSpace c = lexer' cs
    | ',' == c = lexer' cs
    | '[' == c = arrayToken' JArray '[' ']' cs
    | '"' == c = stringToken (/='"') cs
    | '\'' == c = stringToken (/='\'') cs
    | '{' == c = arrayToken' JObject '{' '}' cs
    | ':' == c = JSpecial ":" : lexer' cs
    | isAlpha c = onAlpha (c:cs)
    | isDigit c = onDigit (c:cs)
    | '}' == c = []
    | ']' == c = []
    | otherwise = throw (WrongFileFormat ("on: " ++ [c]))

{-
    Permet de capturer une chaine de caractère
    Arguments:
        1. filtre avec caractère qui délimite une chaine
        2. correspond au json à analyser
    Retourne une liste de tokens
-}
stringToken filter cs = JString token : lexer' rest
    where (token, r:rest) = span filter cs

{-
    Permet de capturer un objet ou un tableau
    Arguments:
        1. json à analyser
        2. caractère ouvrant
        3. caractère fermant
    Retourne un tuple contenant le contenu de l'objet ainsi que le reste du json
-}
ponctuationFilter :: String -> Char -> Char -> (String, String)
ponctuationFilter cs openChar closeChar =
    let
        ponctuationFilter' :: String -> String -> Int -> Bool -> (String, String)
        ponctuationFilter' [] _ _ _ = ("", "")
        ponctuationFilter' (c:cs) k n b
            | c == '"'                  = ponctuationFilter' cs (k++[c]) n (not b)
            | b                         =  ponctuationFilter' cs (k++[c]) n b
            | c == closeChar && n == 0  = (k, cs)
            | c == closeChar            = ponctuationFilter' cs (k++[c]) (n-1) b
            | c == openChar             = ponctuationFilter' cs (k++[c]) (n+1) b
            | otherwise                 = ponctuationFilter' cs (k++[c]) n b
    in
        ponctuationFilter' cs "" 0 False


{-
    Permet de construire un objet ou un tableau
    Arguments:
        1. le type de token (JObject ou JArray)
        2. caractère ouvrant
        3. caractère fermant
        4. json à analyser
    Retourne une liste de token
-}
arrayToken' constructor openChar closeChar cs =
    let
        (keep, rest) = ponctuationFilter cs openChar closeChar
    in
        constructor (lexer' keep) : lexer' rest

{-
    Permet de parser un string en nombre dans un JNumber
    Arguments:
        1. json à analyser
    Retourne une liste de token
-}
onDigit cs = JNumber (read token :: Int): lexer' rest
    where (token, rest) = span isDigit cs

{-
    Permet de parser un null ou un bool
    Arguments:
        1. json à analyser
    Retourne une liste de token
-}
onAlpha cs =
    let
        onAlpha' os
            | os == "true" = JBool True
            | os == "false" = JBool False
            | os == "null" = JNull
            | otherwise = throw (WrongFileFormat ("on: " ++ os))
    in
        onAlpha' token :  lexer' rest
    where (token, rest) = span isAlpha cs

main::IO()
main = do
    args <- getArgs
    let file = head args
    json <- readFile file
    putStr $ show $ lexer json
