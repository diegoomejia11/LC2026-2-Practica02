module Practica02 where

-- Sintaxis de la logica proposicional
data Prop
  = Var String
  | Cons Bool
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop
  deriving (Eq)

instance Show Prop where
  show (Cons True) = "⊤"
  show (Cons False) = "⊥"
  show (Var v) = v
  show (Not a) = "¬" ++ show a
  show (Or a b) = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
  show (And a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
  show (Impl a b) = "(" ++ show a ++ " → " ++ show b ++ ")"
  show (Syss a b) = "(" ++ show a ++ " ↔ " ++ show b ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

-- EJERCICIOS

-- Ejercicio 1
variables :: Prop -> [String]
variables prop = sinRepetidos (recorre prop)
  where
    recorre (Var v) = [v]
    recorre (Cons _) = []
    recorre (Not a) = recorre a
    recorre (And a b) = recorre a ++ recorre b
    recorre (Or a b) = recorre a ++ recorre b
    recorre (Impl a b) = recorre a ++ recorre b
    recorre (Syss a b) = recorre a ++ recorre b

-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var v) estado = pertenece v estado
interpretacion (Cons b) _ = b
interpretacion (Not a) estado = negacion (interpretacion a estado)
interpretacion (And a b) estado = interpretacion a estado && interpretacion b estado
interpretacion (Or a b) estado = interpretacion a estado || interpretacion b estado
interpretacion (Impl a b) estado = negacion (interpretacion a estado) || interpretacion b estado
interpretacion (Syss a b) estado = interpretacion a estado == interpretacion b estado

-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjPotencia (variables prop)

-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos p = [ i| i <- estadosPosibles p, interpretacion p i ] 

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes f g = 
    yeso [interpretacion f e == interpretacion g e | e <- estados]
    where 
    estados = conjPotencia(sinRepetidos(variables g ++ variables f))

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia = undefined

-- Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion = undefined

-- Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined

-- Funciones auxiliares

-- Conjunto potencia de una lista
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x : xs) = [(x : ys) | ys <- conjPotencia xs] ++ conjPotencia xs

-- Elimina repetidos de una lista de Strings
sinRepetidos :: [String] -> [String]
sinRepetidos [] = []
sinRepetidos (x : xs)
  | pertenece x xs = sinRepetidos xs
  | otherwise = x : sinRepetidos xs

-- Pertenencia en lista de Strings
pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece v (x : xs) = v == x || pertenece v xs

-- Negacion de un booleano
negacion :: Bool -> Bool
negacion True = False
negacion False = True

-- Filtro de lista
filtra :: (a -> Bool) -> [a] -> [a]
filtra _ [] = []
filtra f (x : xs)
  | f x = x : filtra f xs
  | otherwise = filtra f xs

yeso [] = True
yeso (x:xs) = x && and xs