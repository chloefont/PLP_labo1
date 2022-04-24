import Control.Monad (unless)
import Control.Monad (when)
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
load (GameState le ri (Boat MyRight onboard)) p = GameState le (onboard:(filter (/= p) ri)) (Boat MyRight p)

unload:: GameState -> GameState
unload gs@(GameState le ri (Boat side None)) = gs
unload gs@(GameState le ri (Boat MyLeft onboard)) = GameState (onboard:le) ri (Boat MyLeft None)
unload gs@(GameState le ri (Boat MyRight onboard)) = GameState le (onboard:ri) (Boat MyRight None)

moveBoat:: GameState -> GameState
moveBoat (GameState le ri (Boat MyLeft onboard)) = GameState le ri (Boat MyRight onboard)
moveBoat (GameState le ri (Boat MyRight onboard)) = GameState le ri (Boat MyLeft onboard)

isLegal:: [Passenger] -> Bool
isLegal [] = True
isLegal [Wolf] = True
isLegal [Corn] = True
isLegal [Sheep] = True
isLegal gs = not ((elem Wolf gs) && (elem Sheep gs) && (elem Corn gs) || (elem Wolf gs) && (elem Sheep gs) || (elem Sheep gs) && (elem Corn gs))

strToPassenger:: String -> Passenger
strToPassenger "loup" = Wolf
strToPassenger "chevre" = Sheep
strToPassenger "choux" = Corn
strToPassenger _ = None

printHelp :: IO()
printHelp = do
    putStrLn ":p afficher l'état du jeu\n\
            \:l <passenger> charger la barque avec un passager\n\
            \:u décharger la barque\n\
            \:m déplacer la barque\n\
            \:r réinitialiser le jeu\n\
            \:q quitter le jeu\n\
            \:h afficher l'aide"

initState = GameState [Wolf, Sheep, Corn] [] (Boat MyLeft None)

main:: IO()
main = do
    mainLoop initState

mainLoop:: GameState -> IO()
mainLoop (GameState [] _ (Boat _ None)) = do putStrLn "Vous avez gagné !"
mainLoop gs = do
    putStrLn "'h' pour afficher l'aide.\nVeuillez entrez une commande:"
    userInput <- getLine
    case userInput of
        'p':_ -> do
            putStrLn $ show gs
            mainLoop gs
        'l':' ':arg -> do
            putStrLn arg
            mainLoop $ load gs $ strToPassenger arg
        'u':_ -> do
            mainLoop $ unload gs
        'm':_ -> do
            let newState = moveBoat gs
            let nextGs = if side(boat(gs)) == MyLeft then
                    if isLegal (left newState) then newState else gs
                else
                    if isLegal (right newState) then newState else gs
            mainLoop nextGs
        'r':_ -> do
            putStrLn "Reinitialisation du jeu..."
            mainLoop initState
        'h':_ -> do
            printHelp
            mainLoop gs
        'q':_ -> do
            putStrLn "Au revoir !"
        _ -> do
            putStrLn "Commande inconnue"
            mainLoop gs