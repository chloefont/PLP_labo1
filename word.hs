{-
Labo 1 - Exercice 2.2
Auteurs : Luca Coduri & Chloé Fontaine
-}

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
      printLine "file" "word" "line" "byte"
      -- On initialise le compteur de ligne, de mot et de caractère à 0
      printCountsFiles (0, 0, 0) args

-- Affiche le nombre de ligne, de mot et de caractère pour tous les fichiers de la liste passée par paramètre
printCountsFiles :: (Int, Int, Int) -> [String] -> IO ()
printCountsFiles (wordTotal, lineTotal, byteTotal) [] = printLine "total" (show wordTotal) (show lineTotal) (show byteTotal)
printCountsFiles (wordTotal, lineTotal, byteTotal) (filename : args) =
  do
    str <- readFile filename
    let (wordCnt, lineCnt, byteCnt) = getCountsStr str
    printLine filename (show wordCnt) (show lineCnt) (show byteCnt)
    printCountsFiles (wordTotal + wordCnt, lineTotal + lineCnt, byteTotal + byteCnt) args

-- Retourne le nombre de mots, de lignes et de caractères d'un texte
getCountsStr :: String -> (Int, Int, Int)
getCountsStr str =
  let text = pack str in
    (Prelude.length $ Data.Text.words text, Prelude.length $ Data.Text.lines text, Prelude.length str)


printLine :: (PrintfArg t1, PrintfArg t2, PrintfArg t3, PrintfArg t4,
 PrintfType t5) => t1 -> t2 -> t3 -> t4 -> t5
printLine filename word line byte =
  do
    printf "%-15s %5s %5s %5s\n" filename word line byte