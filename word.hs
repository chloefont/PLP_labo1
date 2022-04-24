import Data.Text
import System.Environment
import Text.Printf

main :: IO ()
main =
  do
    args <- getArgs
    if Prelude.null args
      then putStrLn "Entrez un nom de fichier"
      else do
        printf "%-10s %5s %5s %5s\n" "file" "word" "line" "byte"
        printCountsFiles (0, 0, 0) args

printCountsFiles :: (Int, Int, Int) -> [String] -> IO ()
printCountsFiles (wordTotal, lineTotal, byteTotal) [] = printf "%-10s %5i %5i %5i\n" "total" wordTotal lineTotal byteTotal
printCountsFiles (wordTotal, lineTotal, byteTotal) (a : args) =
  do
      -- à enlever ?
    let biggerFileLen = Prelude.foldl (\a str -> if Prelude.length str > a then Prelude.length str else a) 0 args
    str <- readFile a
    let (wordCnt, lineCnt, byteCnt) = getCountsStr str

    printf "%-10s %5i %5i %5i\n" a wordCnt lineCnt byteCnt
    printCountsFiles (wordTotal + wordCnt, lineTotal + lineCnt, byteTotal + byteCnt) args

getCountsStr :: String -> (Int, Int, Int)
getCountsStr str =
  let text = pack str in
    (Prelude.length $ Data.Text.words text, Prelude.length $ Data.Text.lines text, Prelude.length str)
