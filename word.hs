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
      printf "%-10s %5s %5s %5s\n" "file" "word" "line" "byte"
      -- On initialise le compteur de ligne, de mot et de caractère à 0
      printCountsFiles (0, 0, 0) args

-- Affiche le nombre de ligne, de mot et de caractère pour tous les fichiers de la liste passée par paramètre
printCountsFiles :: (Int, Int, Int) -> [String] -> IO ()
printCountsFiles (wordTotal, lineTotal, byteTotal) [] = printf "%-10s %5i %5i %5i\n" "total" wordTotal lineTotal byteTotal
printCountsFiles (wordTotal, lineTotal, byteTotal) (a : args) =
  do
    str <- readFile a
    let (wordCnt, lineCnt, byteCnt) = getCountsStr str
    printf "%-10s %5i %5i %5i\n" a wordCnt lineCnt byteCnt
    printCountsFiles (wordTotal + wordCnt, lineTotal + lineCnt, byteTotal + byteCnt) args

-- Retourne le nombre de mots, de lignes et de caractères d'un texte
getCountsStr :: String -> (Int, Int, Int)
getCountsStr str =
  let text = pack str in
    (Prelude.length $ Data.Text.words text, Prelude.length $ Data.Text.lines text, Prelude.length str)
