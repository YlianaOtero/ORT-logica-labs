module Lab3Solution where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Yliana Otero, Joaquin Villanueva
-- Números: 301178, 283473
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

----------------------------------------------------------------------------------
-- Formalización del lenguaje y otros elementos
----------------------------------------------------------------------------------
type Var = String
type Lit = (Var,Bool)
type I = [Lit]

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff
  deriving (Show, Eq)

data Clase = Tau | Contra | Cont
  deriving (Show, Eq)

data Consecuencia = [L] :|= L deriving (Show, Eq)

data Tableau = Conj [L] Tableau
             | Dis  [L] Tableau Tableau
             | Hoja I
  deriving (Eq)

top = Bin (V "p") Or  (Neg $ V "p")
bot = Bin (V "p") And (Neg $ V "p")

listaConsistente =[("p",True),("q",False),("r",True)]
listaNoConsistente = [("p",True),("q",False),("r",True),("p",False)]

-- 1)
-- Pre: recibe una lista de asignaciones de valores de verdad sobre variables
-- Pos: retorna True si y solo si la lista es consistente, o sea representa una interpretación
esConsistente :: I -> Bool
esConsistente [] = True
esConsistente((l,b):xs) = existeElLiteralNegado ((l,b):xs) && esConsistente xs

existeElLiteralNegado :: I -> Bool
existeElLiteralNegado [] = True
existeElLiteralNegado ((l,b):xs) = filter (\(var, bool) -> var == l && bool /= b) xs == []


-- 2)
-- Pre: recibe una interpretación dada como lista de asignaciones (no vacía y consistente) 
-- Pos: retorna la interpretación expresada como una conjunción de literales
int2f :: I -> L
int2f [x] = literal2f x --Caso base
int2f (x:xs) = Bin (literal2f x) And (int2f xs)

literal2f :: Lit -> L
literal2f (v, True) = V v
literal2f (v, False) = Neg (V v)

-- 3)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna el tableau de f
tableau :: L -> Tableau
tableau f = construyeTableau [f] []

--Funcion recursiva que acumula los resultados de aplicar las reglas de teablau
--Se le pasa una lista vacia donde se guardan las formulas
construyeTableau :: [L] -> I -> Tableau

construyeTableau [] i = Hoja i

construyeTableau (x:xs) i =
  case x of
    V var -> 
      construyeTableau xs ((var, True) : i)
    Neg (V var) -> 
      construyeTableau xs ((var, False) : i)
    
    --Reglas conjuntivas
    Bin p And q -> Conj [x] (construyeTableau (p:q:xs) i)
    Neg (Neg p) -> construyeTableau (p:xs) i --Doble negacion
    Neg (Bin p Or q) -> construyeTableau (Neg p:Neg q:xs) i
    Neg (Bin p Imp q) -> construyeTableau (p:Neg q:xs) i

    --Reglas disyuntivas
    Bin p Or q -> Dis [x] (construyeTableau (p:xs) i) (construyeTableau (q:xs) i)
    Bin p Imp q -> Dis [x] (construyeTableau (Neg p:xs) i) (construyeTableau (q:xs) i)
    Neg (Bin p And q) -> Dis [x] (construyeTableau (Neg p:xs) i) (construyeTableau (Neg q:xs) i)

-- 4)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna True si y solo si f es sat
sat :: L -> Bool
sat f = verificaSat (tableau f)

-- Auxiliar
verificaSat :: Tableau -> Bool
verificaSat (Hoja i) = esConsistente i
verificaSat (Conj _ t) = verificaSat t
verificaSat (Dis _ t1 t2) = verificaSat t1 || verificaSat t2

-- 5)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna una lista con todos los modelos completos de f
-- Recomendación: para imprimirlos los modelos en lineas distintas:
--                ghci> mapM_ print $ modelos f
modelos :: L -> [I]
modelos = undefined

-- 6)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna la clase semántica a la que pertenece f
clasificar :: L -> Clase
clasificar = undefined

-- 7)
-- Pre: recibe una consecuencia
-- Pos: retorna la consecuencia expresada como una fórmula de LP
cons2f :: Consecuencia -> L
cons2f = undefined

-- 8)     
-- Pre: recibe una consecuencia
-- Pos: retorna True si y solo si la consecuencia es válida
valida :: Consecuencia -> Bool
valida = undefined

-- 9)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FND
fnd :: L -> L
fnd = undefined

-- 10)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FNC
fnc :: L -> L
fnc = undefined


----------------------------------------------------------------------------------
-- Fórmulas del Lab1 para probar
----------------------------------------------------------------------------------
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


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------

vars :: L -> [Var]
vars (V p)         = [p]
vars (Neg f)       = vars f
vars (Bin f1 _ f2) = nub (vars f1 ++ vars f2)

----------------------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------------------
instance Show L where
  show (V p)           = p
  show (Neg (Neg a))   = "¬" ++ show (Neg a)
  show (Neg (V p))     = "¬" ++ show (V p)
  show (Neg a)         = "¬" ++ show a ++ ""
  show (Bin a And b)   = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin a Or b)    = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b)   = "(" ++ show a ++ " --> " ++ show b ++ ")"
  show (Bin a Iff b)   = "(" ++ show a ++ " <-> " ++ show b ++ ")"

instance Show Tableau where
    show = prettyPrintT

-- Formatear tableau indentado
-- Adaptado de https://stackoverflow.com/a/19442407
prettyPrintT :: Tableau -> String
prettyPrintT t = unlines (prettyPrintAux t)
  where
    prettyPrintAux (Hoja i)       = [show (map lit2f i) ++ if esConsistente i then " O" else " X"]
    prettyPrintAux (Conj l t)     = (show l) : prettyPrintSubTree [t]
    prettyPrintAux (Dis  l t1 t2) = (show l) : prettyPrintSubTree [t1,t2]
    --
    prettyPrintSubTree []     = []
    prettyPrintSubTree [t]    = ((pad "'- " "   ") (prettyPrintAux t))
    prettyPrintSubTree (t:ts) = ((pad "+- " "|  ") (prettyPrintAux t)) ++ prettyPrintSubTree ts
    --
    pad first rest = zipWith (++) (first : repeat rest)
    --
    lit2f (v,b) | b = V v
                | otherwise = Neg (V v)
