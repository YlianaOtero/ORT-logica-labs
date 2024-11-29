module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Yliana Otero Coitiño
-- Números: 301178
----------------------------------------------------

-- IMPORTANTE: se modificaron los nombres de los tipos para que compilaran las pruebas de TestLab1.hs
import Prelude

-- EJERCICIO 1.1 --
-- Completar la definicion del tipo de f́ormulas (L), siguiendo las reglas de sintaxis precedentes.
type Var = String

data V = Top | Bottom | V Var | Neg V | Bin V BC V
  deriving (Show, Eq)
data BC = And | Or | Imp | Bicond
  deriving (Show, Eq)

p :: V
p = V "p"

q :: V
q = V "q"

r :: V
r = V "r"

-- EJERCICIO 1.2 --
-- Codificar y nombrar las siguientes formulas.
--a)
-- p ∧ not not q
fa :: V
fa = Bin (V "p") And (Neg (Neg (V "q")))

--b) 
-- p ∧ not q ∧ not r
fb :: V
fb = Bin (Bin (V "p") And (Neg (V "q"))) And (Neg (V "r"))
--c)
-- (not (not p)) ∨ not (q ∧ p)
fc :: V
fc = Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p")))
--d)
-- not(r ⊃ r) ∧ ((not (not p)) ∨ not (q ∧ p))
fd :: V
fd = Bin (Neg (Bin (V "r") Imp (V "r"))) And (Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p"))))


-- EJERCICIO 1.3 --
--a)
-- Cuenta la cantidad de conectivas binarias de una formula.
cantBin :: V -> Int
cantBin (Bin f1 _ f2) = 1 + cantBin f1 + cantBin f2
cantBin (Neg f) = cantBin f
cantBin _ = 0


--b)
-- Dada una conjuncion de literales (estos son variables sin negar o variables 
-- negadas), devuelve una lista con los nombres de las variables y
-- un valor de verdad asociado. El valor debe ser True si la variable no se
-- encuentra negada y False si se encuentra negada.

extraerV :: V -> Var
extraerV (V v) = v

valores :: V -> [(Var,Bool)]
valores (Bin f1 And f2) = valores f1 ++ valores f2
valores (Bin f1 Or f2) = valores f1 ++ valores f2
valores (Bin f1 Imp f2) = valores f1 ++ valores f2
valores (Bin f1 Bicond f2) = valores f1 ++ valores f2
valores (Neg (V v)) = [(v, False)]
valores (V v) = [(v, True)]
valores _ = []

--c)
-- Simplifica una formula eliminando las negaciones dobles.
dobleNeg :: V -> V
dobleNeg (Neg (Neg f)) = dobleNeg f
dobleNeg (Neg f) = Neg(dobleNeg f)
dobleNeg (Bin f1 b f2) = Bin (dobleNeg f1) b (dobleNeg f2)
dobleNeg f = f


--d)
-- Sustituye la disyuncion (∨) por su equivalente logico: α ∨β ≈ ¬α ⊃β
cambiar :: V -> V
cambiar (Bin f1 Or f2) = Bin (Neg (cambiar f1)) Imp (cambiar f2)
cambiar (Bin f1 b f2) = Bin (cambiar f1) b (cambiar f2)
cambiar (Neg f) = Neg (cambiar f)
cambiar f = f

--e)
-- Cuenta la cantidad de veces que aparece una letra proposicional en una formula.
cantPropX :: V -> Var -> Int
cantPropX (V v) v'
  | v == v' = 1
  | otherwise = 0
cantPropX (Neg f) v = cantPropX f v
cantPropX (Bin f1 b f2) v = cantPropX f1 v + cantPropX f2 v
cantPropX _ _ = 0

--f)
-- Devuelve una lista que contiene las letras proposicionales que aparecen en una formula, sin repetidos.
existe :: Var -> [Var] -> Bool
existe _ [] = False
existe v (x:xs)
  | v == x = True
  | otherwise = existe v xs

eliminarRepetidos :: [Var] -> [Var]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
  | existe x xs = eliminarRepetidos xs
  | otherwise = x : eliminarRepetidos xs

listarPropAux :: V -> [Var]
listarPropAux (V v) = [v]
listarPropAux (Neg f) = listarPropAux f
listarPropAux (Bin f1 _ f2) 
  | f1 == f2 = listarPropAux f1 
  | otherwise = listarPropAux f1 ++ listarPropAux f2
listarPropAux _ = []

listarProp :: V -> [Var]
listarProp f = eliminarRepetidos (listarPropAux f)


--g)
-- Recibe dos conectivas binarias y sustituye la primera por la segunda en una formula.
sustCon :: V -> BC -> BC -> V
sustCon (Bin f1 b f2) bc1 bc2 
  | b == bc1 = Bin (sustCon f1 bc1 bc2) bc2 (sustCon f2 bc1 bc2)
  | otherwise = Bin (sustCon f1 bc1 bc2) b (sustCon f2 bc1 bc2)
sustCon (Neg f) bc1 bc2 = Neg (sustCon f bc1 bc2)
sustCon f bc1 bc2 = f


--h)
-- Idem (g), pero intercambiando ambas conectivas.
swapCon :: V -> BC -> BC -> V
swapCon (Bin f1 b f2) bc1 bc2 
  | b == bc1 = Bin (swapCon f1 bc1 bc2) bc2 (swapCon f2 bc1 bc2)
  | b == bc2 = Bin (swapCon f1 bc1 bc2) bc1 (swapCon f2 bc1 bc2)
  | otherwise = Bin (swapCon f1 bc1 bc2) b (swapCon f2 bc1 bc2)
swapCon (Neg f) bc1 bc2 = Neg (swapCon f bc1 bc2)
swapCon f bc1 bc2 = f

--i)
-- Invierte los valores de las variables (ej. p por ¬p y ¬p por p) y los
-- conectivos de conjuncion/disyuncion (∧ por ∨ y ∨ por ∧). Utilizar
-- dobleNeg para eliminar las dobles negaciones y swapCon para invertir
-- las conectivas binarias.
invertirSimp :: V -> V
invertirSimp (Bin f1 Imp f2) = Bin (invertirSimp f1) Imp (invertirSimp f2)
invertirSimp (Bin f1 Bicond f2) = Bin (invertirSimp f1) Bicond (invertirSimp f2)
invertirSimp (Bin f1 b f2) = Bin (invertirSimp f1) b (invertirSimp f2)
invertirSimp (Neg (Neg f)) = invertirSimp (dobleNeg (Neg (Neg f)))
invertirSimp (Neg f) = f
invertirSimp f = Neg f

invertir :: V -> V
invertir f = invertirSimp (swapCon f And Or)

--j)
-- Recibe una variable p, dos formulas β y α, y devuelve la formula que se
-- obtiene al sustituir p por β cada vez que p aparece en α (esta operacion
-- se nota α[p := β]).
sustSimp :: Var -> V -> V -> V
sustSimp p beta (Bin f1 b f2) = Bin (sustSimp p beta f1) b (sustSimp p beta f2)
sustSimp p beta (Neg f) = Neg (sustSimp p beta f)
sustSimp p beta alfa 
  | alfa == V p = beta
  | otherwise = alfa

--k)
-- Recibe una sustitucion multiple σ y una formula α, y devuelve la
-- formula que se obtiene al efectuar la sustitucion multiple σ sobre α
-- (esta operacion se nota α[σ]).
-- La sustitucion multiple σ es representada por una lista de parejas (p,β)
-- donde p es una letra proposicional y β es una formula. La idea es que
-- cada pareja (p,β) sirve para indicar que p debe sustituirse por β. Si una
-- letra aparece en dicha lista mas de una vez, se considerara solamente
-- su primera aparicion, ignorandose las otras.
sustMult :: [(Var, V)] -> V -> V
sustMult [] f = f
sustMult ((p, beta):xs) alfa = sustMult xs (sustSimp p beta alfa)
