module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Yliana Otero, 
-- Números: 301178, 
----------------------------------------------------

import Prelude
import Data.List (nub)

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V v) = i v 
eval i (Neg f) = not (eval i f)
eval i (Bin f1 Or f2) = (eval i f1) || (eval i f2)
eval i (Bin f1 And f2) = (eval i f1) && (eval i f2)
eval i (Bin f1 Imp f2) = not (eval i f1) || (eval i f2)
eval i (Bin f1 Iff f2) = (eval i f1) == (eval i f2)


--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas _ = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas _ = False

--1.4)
irfalsa :: Var -> Bool
irfalsa "r" = False
irfalsa _ = True

--1.5)
-- Completar con verdadera/falsa:
-- fa es False bajo itodasfalsas
-- fb es False bajo itodasfalsas
-- fc es True bajo itodasfalsas
-- fd es False bajo itodasfalsas
-- 
-- fa es True bajo itodasverdaderas
-- fb es False bajo itodasverdaderas
-- fc es True bajo itodasverdaderas
-- fd es False bajo itodasverdaderas
--
-- fa es True bajo irfalsa
-- fb es False bajo irfalsa
-- fc es True bajo irfalsa
-- fd es False bajo irfalsa

-- Lo hicimos ejecutando eval funcion fi, por ejemplo eval irfalsa fd

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari [] _ = error "La lista original está vacía o la variable no forma parte de la lista."
creari ((varLista, x):xs) v
  | (varLista == v) = x
  | otherwise = creari xs v

--1.7)
-- No, no es la misma porque, si bien en la lista [(r, False),(p,True),(q,True)] la variable r tiene valor False, 
-- y las demás variables de la lista tienen valor True, si paso cualquier otra variable que no esté en la lista no
-- voy a obtener True, sino que voy a obtener un error. La función irfalsa devuelve False si le paso r, y True
-- en cualquier otro caso.


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)

filas :: [Var] -> [Fila]
filas [] = [[]]
filas (p:ps) = (map ((p, True) :) (filas ps)) ++ (map ((p, False) : ) (filas ps))



--2.2)
listarProp :: L -> [Var]
listarProp (V var) = [var]
listarProp (Neg f) = listarProp f
listarProp (Bin f1 _ f2) = nub (listarProp f1 ++ listarProp f2)

tvAux :: L -> Fila -> (Fila, Bool)
tvAux f fila = (fila, eval (creari fila) f)

tv :: L -> TV
tv f = map (tvAux f) (filas (listarProp f))

--2.3)
extraerValoresTV :: TV -> [Bool]
extraerValoresTV [] = []
extraerValoresTV ((_, b):xs) = b : extraerValoresTV xs

todosBooleanosIguales :: [Bool] -> Bool -> Bool
todosBooleanosIguales [] _ = True
todosBooleanosIguales (x:xs) b = x == b && todosBooleanosIguales xs b

es :: L -> Clase -> Bool
es f Tau = todosBooleanosIguales (extraerValoresTV (tv f)) True
es f Contra = todosBooleanosIguales (extraerValoresTV (tv f)) False
es f Cont = not (todosBooleanosIguales (extraerValoresTV (tv f)) True) && not (todosBooleanosIguales (extraerValoresTV (tv f)) False)
es f Sat = not (todosBooleanosIguales (extraerValoresTV (tv f)) False)
es f Fal = not (todosBooleanosIguales (extraerValoresTV (tv f)) True)


--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es contingencia
-- fb es contingencia
-- fc es tautología
-- fd es contradicción

--2.5) 
fnc :: L -> L
fnc f = conjuncionDeDisyunciones (tablaDeVerdadFalsas (tv f))

tablaDeVerdadFalsas :: TV -> [Fila]
tablaDeVerdadFalsas tv = [fila | (fila, val) <- tv, not val]

conjuncionDeDisyunciones :: [Fila] -> L
conjuncionDeDisyunciones [] = error "Es una Tautologia"
conjuncionDeDisyunciones [fila] = disyuncionDeVariables fila
conjuncionDeDisyunciones (fila:filas) = Bin (disyuncionDeVariables fila) And (conjuncionDeDisyunciones filas)

disyuncionDeVariables :: Fila -> L
disyuncionDeVariables [] = error "Fila vacía"
disyuncionDeVariables [(var, valor)] = if valor then Neg (V var) else V var
disyuncionDeVariables ((var, valor):xs) = Bin (if valor then Neg (V var) else V var) Or (disyuncionDeVariables xs)


----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"
