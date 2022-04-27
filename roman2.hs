import Data.Char (toUpper)

rtoa :: Char -> Int
rtoa 'M' = 1000
rtoa 'D' =  500
rtoa 'C' =  100
rtoa 'L' =   50
rtoa 'X' =   10
rtoa 'V' =    5
rtoa 'I' =    1
rtoa r   = error $ "Invalid rtoa char:" ++ show r

urtoa :: Char -> Int
urtoa = rtoa . toUpper

romanToInt :: String -> Int
romanToInt = foldl1 (\t n -> t+n-t`mod`n*2) . map urtoa