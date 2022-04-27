import Data.List (intercalate)

json = ObjectToken [("a", NumberToken 1), ("b", ArrayToken [BoolToken False, StringToken "A"])]

data Token = NullToken
            | BoolToken Bool
            | StringToken String
            | NumberToken Int
            | ArrayToken [Token]
            | ObjectToken [(String, Token)]
            deriving (Eq)

instance Show Token where 
    show value = case value of
        NullToken          -> "null"
        BoolToken True     -> "true"
        BoolToken False    -> "false"
        StringToken s      -> showJSONString s
        NumberToken s      -> show s
        ArrayToken a       -> "[" ++ intercalate ", " (map show a) ++ "]"
        ObjectToken o      -> "{" ++ intercalate ", " (map showKV o) ++ "}"
        where
            showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ s ++ "\""