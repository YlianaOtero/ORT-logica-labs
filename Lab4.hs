------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Lab3

type Nat = Int

----------------------------------------------------------------------------------
-- 1. Veraces y mentirosos
----------------------------------------------------------------------------------

-- Variables proposicionales:
-- ...
--- A Es Caballero 
a = V "a"
--- B Es Caballero
b = V "b"
--- C Es Caballero
c = V "c"


-- Respuesta: ...
ej1 :: L
ej1 = Bin diceAej1 And diceBej1

-- "Todos somos escuderos"
diceAej1 = Bin a Iff (Bin (Bin (Neg a) And (Neg b)) And (Neg c))

-- "Solo uno de nosotros es caballero"
diceBej1 = Bin b Iff (Bin (Bin (Bin a And (Bin (Neg b) And (Neg c))) Or (Bin b And (Bin (Neg a) And (Neg b)))) Or (Bin c And (Bin (Neg a) And (Neg b))))

-- Respuesta: ...
ej2 :: L
ej2 = Bin diceAej2 And diceBej2  

-- "Todos somos escuderos"
diceAej2 = Bin a Iff (Bin (Bin (Neg a) And (Neg b)) And (Neg c))

-- "Solo uno de nosotros es escudero"
diceBej2 = Bin b Iff (Bin (Bin (Bin (Neg a) And (Bin b And c)) Or (Bin (Neg b) And (Bin a And b))) Or (Bin (Neg c) And (Bin a And b)))


-- Respuesta: ...
ej3 :: L
ej3 = Bin diceBej3 And diceCej3

-- "A dijo que es un escudero"

-- B dice la verdad (es caballero), sii (Bin (Neg a) And a), o sea si A dijo que es escudero,
-- pero si A fuese escudero, diría que es caballero porque los escuderos siempre mienten, 
-- y si A fuese caballero diría que es caballero,porque los caballeros siempre dicen la verdad... 
-- por lo tanto es una contradicción.
diceBej3 = Bin b Iff (Bin (Neg a) And a) --Contradiccion

-- "B está mintiendo"
diceCej3 = Bin c Iff (Neg b)
----------------------------------------------------------------------------------
-- 2. Planificación de vigilancia
----------------------------------------------------------------------------------
-- 2.1) Respuesta: ...

-- 2.2) Respuesta: ...

-- 2.3)
-- Pre: recibe 
--        nr - número de robots
--        nz - número de zonas de vigilancia
--        nt - número de franjas temporales
-- Pos: retorna una fórmula de LP formalizando el problema de planificacion básica.
planAB :: Nat -> Nat -> Nat -> L
planAB nr nz nt = undefined                                                      

-- 2.4)
-- Graficar solución en la imagen planAB.png.

-- 2.5)
-- Pre: recibe
--        nr - número de robots
--        nz - número de zonas de vigilancia
--        nt - número de franjas temporales
--        ir - información de rendimiento para cada robot
--        iz - información de importancia para cada zona
-- Pos: retorna una fórmula de LP formalizando el problema de planificacion extendida.
planABC :: Nat -> Nat -> Nat -> [(Nat,Nat)] -> [(Nat,Nat)] -> L
planABC nr nz nt ir iz = undefined

-- 2.6)
-- Información de rendimiento:
infoRobots :: [(Nat,Nat)]
infoRobots = undefined

-- Información de importancia:
infoZonas :: [(Nat,Nat)]
infoZonas = undefined   

-- Graficar solución en la imagen planABC.png.


----------------------------------------------------------------------------------
-- 3. Clique de grafo
----------------------------------------------------------------------------------

type G = (Nat, [(Nat,Nat)])


-- 3.1)
-- Pre: recibe un grafo no dirigido g y un tamaño de clique k
-- Pos: retorna una fórmula de LP formalizando el problema del k-clique 
kClique :: G -> Nat -> L
kClique g@(n,e) k = undefined

-- 3.2)
g8 :: G
g8 = undefined

-- ... Mencionar las soluciones encontradas aquí ...

-- 3.3)
-- ... Mencionar solución encontrada aquí ...

-- 3.4)
-- Pre: recibe un grafo no dirigido g y un tamaño de clique k
-- Pos: retorna una fórmula de LP formalizando el problema del k-clique maximal
maxkClique :: G -> Nat -> L
maxkClique g@(n,e) k = undefined

-- 3.5) 
-- ... Mencionar solución encontrada aquí ...


----------------------------------------------------------------------------------
-- Funciones sugeridas
----------------------------------------------------------------------------------

-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> L) -> L
bigAnd is f = undefined

-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr is f = undefined

-- Variable proposicional indexada
v :: Var -> Nat -> L
v p i = undefined

-- Variable proposicional triplemente indexada
v3 :: Var -> Nat -> Nat -> Nat -> L
v3 p i j k = undefined


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------

-- Pre: recibe un nombre de variable p y un natural n.
-- Pos: genera todas las posibles declaraciones de variables proposicionales 
--      indexadas en el rango 1..n y en el formato SMT-LIB.
--      Por ejemplo, si n=4 tenemos 4 declaraciones de variables.
-- RECOMENDACIÓN: para imprimir en consola ejecutar (si el nombre es "p" y n=4):  
--      ghci> putStrLn (genVars "p" 4)   
genVars :: String -> Nat -> String
genVars p n = foldr (\v b -> ("(declare-fun " ++ (show v) ++ " () Bool)\n") ++ b) "" vars
  where 
    vars = [V (p ++ (show i)) | i <- [1..n]]

-- Pre: recibe un nombre de variable p y dos naturales n y m.
-- Pos: genera todas las posibles permutaciones de declaraciones de variables proposicionales
--      triplemente indexadas en el rango 1..n, 1..m y 1..l en el formato SMT-LIB.
--      Por ejemplo, si n=4, m=2 y l=3 tenemos 4*2*3=24 declaraciones de variables.
-- RECOMENDACIÓN: para imprimir en consola ejecutar (si el nombre es "p", n=4, m=2 y l=3):  
--      ghci> putStrLn (genVars3 "p" 4 2 3)   
genVars3 :: String -> Nat -> Nat -> Nat -> String
genVars3 p n m l = foldr (\v b -> ("(declare-fun " ++ (show v) ++ " () Bool)\n") ++ b) "" vars
  where 
    vars = [V (p ++ (show i) ++ "_" ++ (show j) ++ "_" ++ (show k)) | i <- [1..n], j <- [1..m], k <- [1..l]]    

-- Pre: recibe el dominio de una función finita y la función representada por una lista de parejas (una tabla).
-- Pos: retorna una relación de orden, representada por una lista de parejas, sobre los elementos del 
--      dominio de la funcion basándose en el rango asignado.
--      El orden será irreflexivo, pero no necesariamente total.
orden :: [Nat] -> [(Nat,Nat)] -> [(Nat,Nat)]
orden dom fun = [(x1,x2) | x1 <- dom, x2 <- dom, (crearf fun) x1 < (crearf fun) x2] 

-- Pre: recibe una función finita representada por una lista de parejas (una tabla).
-- Pos: retorna una funcion de haskell que se corresponde con la tabla recibida.
crearf :: [(Nat, Nat)] -> (Nat -> Nat)
crearf []         v = error $ show v ++ " indefinida!"
crearf ((d,r):xs) v = case v == d of
                        True  -> r
                        False -> crearf xs v 

-- Pre: recibe una fórmula de LP.
-- Pos: pretty printing de la fórmula en formato SMT-LIB, esto es: parentizada y prefija.
toPrefix :: L -> String
toPrefix (V p)       = p
toPrefix (Neg a)     = "(not " ++ toPrefix a ++ ")"
toPrefix (Bin a And b) = "(and " ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Or  b) = "(or "  ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Imp b) = "(=> "  ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Iff b) = "(= "   ++ toPrefix a ++ " " ++ toPrefix b ++ ")"