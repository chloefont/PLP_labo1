import Control.Monad (unless)
-- Le loup, la chèvre et les choux (lcc)

data Passenger = Wolf | Sheep | Corn | None
  deriving (Eq, Show)

data Side = MyLeft | MyRight deriving (Eq, Show)
data Boat = Boat{ side::Side, passenger::Passenger } deriving (Show)
data GameState = GameState { left :: [Passenger], right :: [Passenger], boat:: Boat } deriving (Show)

load:: GameState -> Passenger -> GameState
load gs None = gs
load gs@(GameState _ [] (Boat MyRight onboard)) _ = gs
load gs@(GameState [] _ (Boat MyLeft onboard)) _ = gs
load (GameState le ri (Boat MyLeft None)) p = GameState (filter (/= p) le) ri (Boat MyLeft p)
load (GameState le ri (Boat MyRight None)) p = GameState le (filter (/= p) le) (Boat MyRight p)
load (GameState le ri (Boat MyLeft onboard)) p = GameState (onboard:(filter (/= p) le)) ri (Boat MyLeft p)
load (GameState le ri (Boat MyRight onboard)) p = GameState le (onboard:(filter (/= p) le)) (Boat MyRight p)

unload:: GameState -> GameState
unload gs@(GameState le ri (Boat side None)) = gs
unload gs@(GameState le ri (Boat MyLeft onboard)) = GameState (onboard:le) ri (Boat MyLeft None)
unload gs@(GameState le ri (Boat MyRight onboard)) = GameState le (onboard:ri) (Boat MyRight None)

moveBoat:: GameState -> GameState
moveBoat (GameState le ri (Boat MyLeft onboard)) = GameState le ri (Boat MyRight onboard)
moveBoat (GameState le ri (Boat MyRight onboard)) = GameState le ri (Boat MyLeft onboard)

isLegal:: [Passenger] -> Bool
isLegal [] = True


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