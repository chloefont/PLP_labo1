
data Nat =
    Z
    | S Nat

ex = S(S(S(Z)))
ex2 = S(S(Z))

instance Show Nat where
    show Z = "Z"
    show (S n) = "S(" ++ show n ++ ")"

-- Comparaisons

equ :: Nat -> Nat -> Bool
equ Z Z = True
equ Z _ = False
equ _ Z = False
equ (S n) (S m) = equ n m

notEqu :: Nat -> Nat -> Bool
notEqu n m = not $ equ n m

le :: Nat -> Nat -> Bool
le Z Z = False
le Z _ = True
le _ Z = False
le (S n) (S m) = le n m

leq :: Nat -> Nat -> Bool
leq Z Z = True
leq Z _ = True
leq _ Z = False
leq (S n) (S m) = leq n m

be :: Nat -> Nat -> Bool
be n m = not $ leq n m

beq :: Nat -> Nat -> Bool
beq n m = not $ le n m

-- Arithmetic

add :: Nat -> Nat -> Nat
add Z n = n
add (S n) m = S (add n m)

sub :: Nat -> Nat -> Nat
sub Z _ = Z
sub (S n) Z = S n
sub (S n) (S m) = sub n m

mult :: Nat -> Nat -> Nat
mult Z _ = Z
mult (S n) m = if notEqu n Z then add (mult n m) m else m

pow :: Nat -> Nat -> Nat
pow Z _ = Z
pow _ Z = S Z
pow n (S m) = if notEqu m Z then mult (pow n m) n else n

-- Conversions

convertNatToInt :: Nat -> Integer
convertNatToInt Z = 0
convertNatToInt (S n) = 1 + convertNatToInt n

convertIntToNat :: Integer -> Nat
convertIntToNat 0 = Z
convertIntToNat n = S $ convertIntToNat $ n - 1

convertNatToString :: Nat -> String
convertNatToString = show

-- Functions
-- TODO vérifier si c'est juste
zero :: Nat
zero = Z

succ :: Nat -> Nat
succ Z = Z
succ (S n) = n

pred :: Nat -> Nat
pred = S

isZero :: Nat -> Bool
isZero n = equ n Z