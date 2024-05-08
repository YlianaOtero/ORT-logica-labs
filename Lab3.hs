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

--Construye el Tableau recursivo
construyeTableau :: [L] -> I -> Tableau

construyeTableau [] i = Hoja i  --No hay más para procesar, se crea una hoja

construyeTableau (x:xs) i =
  case x of
    V var -> if esConsistente ((var, True) : i)
             then construyeTableau xs ((var, True) : i)
             else Hoja i  --Hay una Inconsistencia, se termina la rama
    Neg (V var) -> if esConsistente ((var, False) : i)
                   then construyeTableau xs ((var, False) : i)
                   else Hoja i  --Hay una Inconsistencia, se termina la rama
    Bin p And q -> construyeTableau (p:q:xs) i  -- Se expande el AND agregando ambas subfórmulas
    Bin p Or q -> Dis [x] (construyeTableau (p:xs) i) (construyeTableau (q:xs) i)  -- Se expande el OR en dos ramas con Dis segun la regla
    Bin p Imp q -> construyeTableau (Bin (Neg p) Or q : xs) i  --p Imp q equivale a ¬p∨q
    Bin p Iff q -> Dis [x] (construyeTableau (Bin p And q : xs) i) (construyeTableau (Bin (Neg p) And (Neg q) : xs) i) --p Iff q equivale a (p∧q)∨(¬p∧¬q)
    
    Neg (Neg p) -> construyeTableau (p:xs) i  -- Doble negación
    Neg (Bin p And q) -> construyeTableau (Bin (Neg p) Or (Neg q) : xs) i  -- De Morgan
    Neg (Bin p Or q) -> construyeTableau (Bin (Neg p) And (Neg q) : xs) i  -- De Morgan
    Neg (Bin p Imp q) -> construyeTableau (Bin p And (Neg q) : xs) i  
    Neg (Bin p Iff q) -> construyeTableau (Bin (Bin p And (Neg q)) Or (Bin (Neg p) And q) : xs) i

--tableau (V p) = Hoja [(p, True)] --Literal
--tableau (Neg (Neg (V p))) = Hoja [(p, True)] --Doble Negacion
--tableau (Neg (V p)) = Hoja [(p, False)] --Negacion
--tableau (Neg (Bin izq Or der)) = Conj [Neg izq, Neg der] (tableau (Bin (Neg izq) And (Neg der))) --Negacion de la disyuncion
--tableau (Neg (Bin izq And der)) = Dis [Neg izq, Neg der] (tableau (Neg izq)) (tableau (Neg der)) --Negacion de la conjuncion
--tableau (Neg (Bin izq Imp der)) = Conj [izq, Neg der] (tableau (Bin izq And (Neg der))) --Negacion de la implicacion
--tableau (Bin izq Or der) = Dis [izq, der] (tableau (izq)) (tableau (der)) --Disyuncion
--tableau (Bin izq Imp der) = Dis [Neg izq, der] (tableau (Neg izq)) (tableau (der)) --Implicacion
--tableau (Neg (Bin izq Iff der)) = Dis [Neg izq, der] tableau 
--tableau (Bin izq And der) = Conj [izq, der] (tableau (Bin (tableau izq) And (tableau der)))
--tableau (Bin izq Iff der) = Dis [izq, der] 

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
