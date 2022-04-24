import Control.Exception

data NotConvertable = NotRomanNumber | NotArabicNumber | NumberTooBig
  deriving (Show)

instance Exception NotConvertable

numberToRoman :: Int -> [Char]
numberToRoman 0 = dictRoman 0
numberToRoman n = if n >= 4000 then throw NumberTooBig else numberToRoman' n

numberToRoman' 0 = ""
numberToRoman' nb = concat (replicate (nb `div` smallest) (dictRoman smallest)) ++ numberToRoman' (nb `mod` smallest)
  where
    smallest = findSmaller nb divs

    findSmaller :: Int -> [Int] -> Int
    findSmaller _ [] = 1
    findSmaller x (l : ls) = if l <= x then l else findSmaller x ls

    divs = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

dictRoman :: (Eq a, Num a) => a -> [Char]
dictRoman x = case x of
  0 -> "0"
  1 -> "I"
  4 -> "IV"
  5 -> "V"
  9 -> "IX"
  10 -> "X"
  40 -> "XL"
  50 -> "L"
  90 -> "XC"
  100 -> "C"
  400 -> "CD"
  500 -> "D"
  900 -> "CM"
  1000 -> "M"
  _ -> throw NotRomanNumber

romanToNumber :: [Char] -> Int
romanToNumber ls = if length (takeWhile (== 'M') ls) >= 4 then throw NumberTooBig else romanToNumber' ls

romanToNumber' :: Num a => [Char] -> a
romanToNumber' [] = 0
romanToNumber' [l] = dictNumber [l]
romanToNumber' (l1 : l2 : ls) = if dictNumber [l1, l2] /= 0 then dictNumber [l1, l2] + romanToNumber' ls else dictNumber [l1] + romanToNumber' (l2 : ls)

--- TODO lever exception si autre caractère
dictNumber :: Num p => [Char] -> p
dictNumber x = case x of
  "0" -> 0
  "I" -> 1
  "IV" -> 4
  "V" -> 5
  "IX" -> 9
  "X" -> 10
  "XL" -> 40
  "L" -> 50
  "XC" -> 90
  "C" -> 100
  "CD" -> 400
  "D" -> 500
  "CM" -> 900
  "M" -> 1000
  _ -> 0