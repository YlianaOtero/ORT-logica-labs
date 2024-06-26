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
-- Una planificación básica requiere que cada robot vigile exactamente una zona de vigilancia en una franja temporal. Si hay más robots que zonas de vigilancia,
-- habrá robots que no tengan zonas asignadas, o robots que tengan más de una zona asignada en una franja temporal, lo cual no es posible en una planificación básica.

-- ii)
-- Sí. Es posible realizar una planificación básica con más zonas de vigilancia que robots.
-- En una planificación básica, cada robot debe vigilar una zona de vigilancia. Si hay más zonas de vigilancia que robots,
-- habrá zonas de vigilancia sin robots asignados, lo cual es posible en una planificación básica.

-- 2.2) Respuesta: ...

-- No es posible que un robot vigile más de una zona en una misma franja temporal. Esto se debe a que en una planificación básica, cada robot debe vigilar
-- una zona de vigilancia. 

-- 2.3)
-- Pre: recibe 
--        nr - número de robots
--        nz - número de zonas de vigilancia
--        nt - número de franjas temporales
-- Pos: retorna una fórmula de LP formalizando el problema de planificacion básica.

-- Condicion A: Todo robot en cualquier momento vigila una y solo una zona.
condA :: Nat -> Nat -> Nat -> L
condA nr nz nt = bigAnd [1..nr] (\robot -> bigAnd [1..nt] (\tiempo -> aplicarEnUnaUnicaZona nz (\zona -> v3 "p" robot zona tiempo)))

aplicarEnUnaUnicaZona :: Nat -> (Nat -> L) -> L
aplicarEnUnaUnicaZona nz f = Bin (bigOr [1..nz] f) And (bigAnd [zona1 | zona1 <- [1..nz]] (\zona1 -> bigAnd [zona2 | zona2 <- [1..nz], zona2 > zona1] (\zona2 -> Bin (Neg (f zona1)) Or (Neg (f zona2)))))


-- Condicion B: No existe más de un robot en la misma zona en una misma franja temporal.
condB :: Nat -> Nat -> Nat -> L
condB nr nz nt = bigAnd [1..nz] (\zona -> bigAnd [1..nt] (\tiempo -> noHayMasDeUnRobot nr zona tiempo))

noHayMasDeUnRobot :: Nat -> Nat -> Nat -> L
noHayMasDeUnRobot nr zona tiempo = 
    bigAnd [1..nr] (\robot1 -> 
    bigAnd [robot2 | robot2 <- [1..nr], robot2 /= robot1] (\robot2 -> 
    Bin (Neg (v3 "p" robot1 zona tiempo)) Or (Neg (v3 "p" robot2 zona tiempo))
    ))

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


-- Paso 1: Ordenar robots y zonas por rendimiento e importancia respectivamente.
-- Paso 2: Asignar robots a zonas de acuerdo a su rendimiento e importancia.
-- Paso 3: Formalizar la asignación de robots a zonas en una fórmula de LP en planABC

planABC :: Nat -> Nat -> Nat -> [(Nat,Nat)] -> [(Nat,Nat)] -> L
planABC nr nz nt ir iz = Bin (Bin (condA nr nz nt) And (condB nr nz nt)) And (condC nr nz nt (sortTuple ir) (sortTuple iz))

-- Ordena de forma descendente por importancia o rendimiento. 
sortTuple :: [(Nat,Nat)] -> [(Nat,Nat)]
sortTuple [] = []
sortTuple (x:xs) = sortBy (\(_,a) (_,b) -> compare b a) (x:xs)

-- Condicion C: Asignar robots a zonas de acuerdo a su rendimiento e importancia. 
-- ir y iz son listas de tuplas (Nat,Nat) donde el primer elemento es el índice del robot/zona y el segundo elemento es el rendimiento/importancia, y
-- están ordenadas de forma descendente por rendimiento/importancia.
condC :: Nat -> Nat -> Nat -> [(Nat,Nat)] -> [(Nat,Nat)] -> L
condC nr nz nt ir iz = bigAnd [1..nt] $ \k -> bigAnd [1..min nr nz] $ \n -> let (i, _) = ir !! (n - 1)
                                                                                (j, _) = iz !! (n - 1)
                                                                            in v3 "p" i j k

-- 2.6)
-- Información de rendimiento:
infoRobots :: [(Nat,Nat)]
infoRobots = [(1, 200), (2, 150), (3, 100)]

-- Información de importancia:
infoZonas :: [(Nat,Nat)]
infoZonas = [(1, 100), (2, 230), (3, 100)]

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
taut = Bin (Neg (V "p")) Or (V "p")
cont = Bin (Neg (V "p") ) And (V "p")


-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> L) -> L
bigAnd [] f =  taut
bigAnd [i] f = f i
bigAnd (i:is) f = Bin (f i) And (bigAnd is f)


-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr [] f =  cont
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