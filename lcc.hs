import Control.Monad (unless)
-- Le loup, la chèvre et les choux (lcc)

printHelp :: IO()
printHelp = do
    putStrLn ":p afficher l'état du jeu\n\
            \:l <passenger> charger la barque avec un passager\n\
            \:u décharger la barque\n\
            \:m déplacer la barque\n\
            \:r réinitialiser le jeu\n\
            \:q quitter le jeu\n\
            \:h afficher l'aide"


main:: IO()
main = do
    putStrLn "'h' pour afficher l'aide"
    mainLoop

mainLoop:: IO()
mainLoop = do
    userInput <- getChar
    _ <- getChar
    case userInput of
        'h' -> do
            printHelp
        'q' -> do
            putStrLn "Au revoir !"
        _ -> do
            putStrLn "Commande inconnue"
    unless (userInput == 'q') mainLoop