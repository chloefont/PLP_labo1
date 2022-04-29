{-
Labo 1 - Exercice 2.5
Auteurs : Luca Coduri & Chloé Fontaine

remarques:
    Lorsqu'un joueur souhaite faire un mouvement non autorisé, le mouvement n'est tout simplement pas effectué.
    Il n'est donc pas possible de perdre à ce jeu.
-}


data Passenger = Wolf | Sheep | Cabbage | None
    deriving (Eq, Show)

data Side = MyLeft | MyRight deriving (Eq, Show)
data Boat = Boat{ side::Side, passenger::Passenger } deriving (Show)
data GameState = GameState { left :: [Passenger], right :: [Passenger], boat:: Boat } deriving (Show)

-- Charger le bateau.
load:: GameState -> Passenger -> GameState
load gs None = gs
load gs@(GameState _ [] (Boat MyRight _)) _ = gs
load gs@(GameState [] _ (Boat MyLeft _)) _ = gs
load (GameState le ri (Boat MyLeft None)) p = GameState (filter (/= p) le) ri (Boat MyLeft p)
load (GameState le ri (Boat MyRight None)) p = GameState le (filter (/= p) le) (Boat MyRight p)
-- Quand le bateau est plein on échange les places.
load (GameState le ri (Boat MyLeft onboard)) p = GameState (onboard:filter (/= p) le) ri (Boat MyLeft p)
load (GameState le ri (Boat MyRight onboard)) p = GameState le (onboard:filter (/= p) ri) (Boat MyRight p)

-- Décharger le bateau.
unload:: GameState -> GameState
unload gs@(GameState le ri (Boat side None)) = gs
unload gs@(GameState le ri (Boat MyLeft onboard)) = GameState (onboard:le) ri (Boat MyLeft None)
unload gs@(GameState le ri (Boat MyRight onboard)) = GameState le (onboard:ri) (Boat MyRight None)

-- Permet de déplacer le bateau à gauche ou à droite.
moveBoat:: GameState -> GameState
moveBoat (GameState le ri (Boat MyLeft onboard)) = GameState le ri (Boat MyRight onboard)
moveBoat (GameState le ri (Boat MyRight onboard)) = GameState le ri (Boat MyLeft onboard)

-- Vérifie si l'état d'une rive est autorisée.
isLegal:: [Passenger] -> Bool
isLegal [] = True
isLegal [Wolf] = True
isLegal [Cabbage] = True
isLegal [Sheep] = True
isLegal gs = not (elem Wolf gs && elem Sheep gs && elem Cabbage gs || elem Wolf gs && elem Sheep gs || elem Sheep gs && elem Cabbage gs)

-- Converti une string en passenger.
strToPassenger:: String -> Passenger
strToPassenger "loup" = Wolf
strToPassenger "chevre" = Sheep
strToPassenger "chou" = Cabbage
strToPassenger _ = None

-- Affiche l'aide.
printHelp :: IO()
printHelp = do
    putStrLn ":p afficher l'état du jeu\n\
            \:l <loup|chevre|chou> charger la barque avec un passager\n\
            \:u décharger la barque\n\
            \:m déplacer la barque\n\
            \:r réinitialiser le jeu\n\
            \:q quitter le jeu\n\
            \:h afficher l'aide"

-- L'état de base d'une partie
initState = GameState [Wolf, Sheep, Cabbage] [] (Boat MyLeft None)

main:: IO()
main = do
    mainLoop initState
{-
    La boucle principale du jeu
    Elle attend les entrées utilisateurs puis execute l'action qui correspond.
    Arguments:
        1. L'etat du jeu
-}
mainLoop:: GameState -> IO()
mainLoop (GameState [] _ (Boat _ None)) = do putStrLn "Vous avez gagné !"
mainLoop gs = do
    putStrLn "'h' pour afficher l'aide.\nVeuillez entrer une commande:"
    userInput <- getLine
    case userInput of
        'p':_ -> do
            print gs
            mainLoop gs
        'l':' ':arg -> do
            putStrLn arg
            mainLoop $ load gs $ strToPassenger arg
        'u':_ -> do
            mainLoop $ unload gs
        'm':_ -> do
            let newState = moveBoat gs
            let nextGs
                  | side(boat gs) == MyLeft =
                    if isLegal (left newState) then newState else gs
                            | isLegal (right newState) = newState
                            | otherwise = gs
            mainLoop nextGs
        'r':_ -> do
            putStrLn "Reinitialisation du jeu...\n\n"
            mainLoop initState
        'h':_ -> do
            printHelp
            mainLoop gs
        'q':_ -> do
            putStrLn "Au revoir !"
        _ -> do
            putStrLn "Commande inconnue"
            mainLoop gs
