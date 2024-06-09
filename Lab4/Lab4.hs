------------------- Estudiante/s -------------------
-- Nombres y apellidos: Yliana Otero, Joaquin Villanueva
-- Números: 301178, 283473
----------------------------------------------------

import Prelude
import Data.List
import Lab3
import qualified Data.Bits as B

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
diceAej1 = Bin a Iff (Neg (Bin (Bin a Or b) Or c))

-- "Solo uno de nosotros es caballero"
diceBej1 = Bin b Iff (Bin (Bin (Bin a And (Bin (Neg b) And (Neg c))) Or (Bin b And (Bin (Neg a) And (Neg b)))) Or (Bin c And (Bin (Neg a) And (Neg b))))

--Una interpretacion posible: modelos ej1
--[("c", False), ("b", False), ("a", True)] 
--A es caballero, B y C son escuderos. 
--Esto cumple con B, que miente sobre que solo uno es caballero 
--(ya que hay uno que es caballero, A).

-- Respuesta: ...
ej2 :: L
ej2 = Bin diceAej2 And diceBej2

-- "Todos somos escuderos"
diceAej2 = Bin a Iff (Bin (Bin (Neg a) And (Neg b)) And (Neg c))

-- "Solo uno de nosotros es escudero"
diceBej2 = Bin b Iff (Bin (Bin (Bin (Neg a) And (Bin b And c)) Or (Bin (Neg b) And (Bin a And b))) Or (Bin (Neg c) And (Bin a And b)))

--Similar al anterior, una interpretacion que devuelve modelos ej2:
--[("c", True), ("b", True), ("a", False)] 
--A es escudero, B y C son caballeros. 
--B miente diciendo que solo uno es escudero, cuando hay uno.

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

--Ejecutando modelos ej3, una interpretacion posible es:
--[("b", False), ("c", True), ("a", True)] 
--A y C son caballeros; B es escudero y miente sobre lo que A dijo.

----------------------------------------------------------------------------------
-- 2. Planificación de vigilancia
----------------------------------------------------------------------------------

-- Para garantizar la seguridad en grandes espacios de manera eficiente han surgido sistemas de vigilancia auto ́nomos con los cuales vigilantes de seguridad robotizados (de aqu ́ı en m ́as, robots) se ocupan de la proteccio ́n perimetral patrullando sin ayuda humana y detectando objetos, actividades o incluso temperat- uras an ́omalas.2 La efectividad de estos sistemas de vigilancia depende, en parte, de una planificaci ́on apropiada entre los distintos robots disponibles y las distintas zonas a vigilar.
-- En este ejercicio nos interesa modelar los posibles escenarios de un sistema de vigilancia que consiste en r robots, z zonas de vigilancia y t franjas temporales. Dichos valores conforman los para ́metros del problema. Por ejemplo, si tenemos r = 2, z = 2 y t = 4, las figuras 1 y 2 nos muestran dos de las posibles planificaciones de vigilancia en la cual se cumplen las siguientes condiciones obviamente deseables:
-- A. Todo robot en cualquier momento vigila alguna zona. B. Nunca asignamos m ́as de un robot en la misma zona.

-- 2.1) Respuesta: ...

-- i)
-- No. No es posible realizar una planificación básica con más robots que zonas de vigilancia. 
-- Una planificación básica requiere que cada robot vigile exactamente una zona de vigilancia. Si hay más robots que zonas de vigilancia,
-- habrá robots que no tengan zonas asignadas, o robots que tengan más de una zona asignada, lo cual no es posible en una planificación básica.


-- CONFLICTO:
-- Si hay más robots que zonas de vigilancia, habrá robots que no tengan zonas asignadas, o robots que tengan más de una zona asignada en una misma franja temporal.
-- Por lo tanto, o bien 2.1) ii) es verdadera y 2.2) es falsa, o 2.1) ii) es falsa y 2.2) es verdadera. 
-- La letra no me da suficiente información... Sin embargo, si no pueden haber zonas sin robots asignados, entonces planAB 3 4 5 es insatisfacible! 
-- Esto no tiene mucho sentido porque en el ejercicio 2.4 me piden que grafique la solución de planAB 3 4 5, lo cual no sería posible si es insatisfacible, o sea,
-- no habría nada para graficar....


-- RESPUESTAS ANTERIORES (revisar)
-- ii)
-- Sí. Es posible realizar una planificación básica con más zonas de vigilancia que robots.
-- En una planificación básica, cada robot debe vigilar una zona de vigilancia. Si hay más zonas de vigilancia que robots,
-- habrá robots vigilando más de una zona de vigilancia en una misma franja temporal. (REVISAR)

-- 2.2) Respuesta: ...

-- La letra no me da suficiente información... Si un robot es físicamente capaz de vigilar más de una zona de vigilancia en una misma franja temporal,
-- entonces la afirmación es verdadera. De lo contrario, es falsa. (REVISAR)

-- 2.3)
-- Pre: recibe 
--        nr - número de robots
--        nz - número de zonas de vigilancia
--        nt - número de franjas temporales
-- Pos: retorna una fórmula de LP formalizando el problema de planificacion básica.

-- Condicion A: Todo robot en cualquier momento vigila alguna zona.
condA :: Nat -> Nat -> Nat -> L
condA nr nz nt = bigAnd [i | i <- [1..nr]] (\i -> bigAnd [k | k <- [1..nt]] (\k -> bigOr [j | j <- [1..nz]] (\j -> v3 "p" i j k)))

-- Condicion B: No existe más de un robot en la misma zona en una misma franja temporal.
condB :: Nat -> Nat -> Nat -> L
condB nr nz nt = bigAnd [j | j <- [1..nz]] (\j -> bigAnd [k | k <- [1..nt]] (\k -> bigOr [i | i <- [1..nr]] (\i -> bigOr [l | l <- [1..nr], l /= i] (\l -> Bin (Neg (v3 "p" i j k)) And (Neg (v3 "p" l j k))))))

-- Condicion extra: Un robot solo puede vigilar una zona en una franja temporal. Esto podemos usarlo si vemos que al final un robot NO puede
-- vigilar más de una zona en una misma franja temporal... Pero agregar esta condición a planAB hace que el ejercicio 2.4 no tenga sentido
-- porque planAB 3 4 5 sería insatisfacible... (REVISAR)
condC :: Nat -> Nat -> Nat -> L
condC nr nz nt = bigAnd [i | i <- [1..nr]] (\i -> bigAnd [j | j <- [1..nz]] (\j -> bigAnd [k | k <- [1..nt]] (\k -> bigAnd [l | l <- [1..nt], l /= k] (\l -> Bin (Neg (v3 "p" i j k)) And (Neg (v3 "p" i j l))))))

planAB :: Nat -> Nat -> Nat -> L
planAB nr nz nt = Bin (condA nr nz nt) And (condB nr nz nt)



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
bigAnd [i] f = f i
bigAnd (i:is) f = Bin (f i) And (bigAnd is f)


-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr [i] f = f i
bigOr (i:is) f = Bin (f i) Or (bigOr is f)

-- Variable proposicional indexada
v :: Var -> Nat -> L
v p i = V (p ++ show i)

-- Variable proposicional triplemente indexada
v3 :: Var -> Nat -> Nat -> Nat -> L
v3 p i j k = V (p ++ show i ++ "_" ++ show j ++ "_" ++ show k)


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