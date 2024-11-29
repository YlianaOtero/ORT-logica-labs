{-# OPTIONS_GHC -fno-warn-tabs #-}
module TestLab1 where

import Lab1        -- Nombre del archivo .hs de la solución
import Test.HUnit  -- Acerca de HUnit: https://hackage.haskell.org/package/HUnit
import Data.List

-- Para ejecutar todas las pruebas usar: runTestTT allTests 
-- runTesTT $ TestList (cantBinTest ++ sustConTests) para correr solo esos dos


form1_cantBin_test = TestCase (assertEqual "cantBin form1" 0 (cantBin form1))
form2_cantBin_test = TestCase (assertEqual "cantBin form2" 2 (cantBin form2))
form3_cantBin_test = TestCase (assertEqual "cantBin form3" 1 (cantBin form3))
form4_cantBin_test = TestCase (assertEqual "cantBin form4" 2 (cantBin form4))
form5_cantBin_test = TestCase (assertEqual "cantBin form5" 3 (cantBin form5))


form1_valores_test  = TestCase (assertBool "valores form1" $ sameList [("p",True)] (valores form1))
form2_valores_test  = TestCase (assertBool "valores form2" $ sameList [("q",True),("r",True),("p",True)] (valores form2))
form3_valores_test  = TestCase (assertBool "valores form3" $ sameList [("q",False),("r",True)] (valores form3))
form4_valores_test  = TestCase (assertBool "valores form4" $ sameList [("p",False),("q",False),("r",True)] (valores form4))
form5_valores_test  = TestCase (assertBool "valores form5" $ sameList [("p",True),("q",True),("p",True),("p",True)] (valores form5))


form11_dobleNeg_test = TestCase (assertEqual "cantBin form11" (V "p") (dobleNeg form11))
form12_dobleNeg_test = TestCase (assertEqual "cantBin form12" (Bin (Bin q And r) Imp p) (dobleNeg form12))
form13_dobleNeg_test = TestCase (assertEqual "cantBin form13" (Bin (Neg q) Or r) (dobleNeg form13))
form14_dobleNeg_test = TestCase (assertEqual "cantBin form14" (Bin (Bin (Neg p) Or (Neg q)) Or r) (dobleNeg form14))
form15_dobleNeg_test = TestCase (assertEqual "cantBin form15" (Bin (Bin (Bin p Imp q) Imp p) Imp p) (dobleNeg form15))


form1_cambiar_test = TestCase (assertEqual "cambiar form1" p (cambiar form1))
form2_cambiar_test = TestCase (assertEqual "cambiar form2" (Bin (Bin q And r) Imp p) (cambiar form2))
form3_cambiar_test = TestCase (assertEqual "cambiar form3" (Bin (Neg(Neg q)) Imp r)  (cambiar form3))
form4_cambiar_test = TestCase (assertEqual "cambiar form4" (Bin (Neg(Bin (Neg(Neg p)) Imp (Neg q))) Imp r)  (cambiar form4))
form5_cambiar_test = TestCase (assertEqual "cambiar form5" (Bin (Bin (Bin p Imp q) Imp p) Imp p) (cambiar form5))


form1_cantPropX_test = TestCase (assertEqual "cantPropX form1" 0 (cantPropX form1 "r"))
form2_cantPropX_test = TestCase (assertEqual "cantPropX form2" 1 (cantPropX form2 "p"))
form3_cantPropX_test = TestCase (assertEqual "cantPropX form3" 0 (cantPropX form3 "p"))
form4_cantPropX_test = TestCase (assertEqual "cantPropX form4" 1 (cantPropX form4 "p"))
form5_cantPropX_test = TestCase (assertEqual "cantPropX form5" 3 (cantPropX form5 "p"))


form1_listarProp_test  = TestCase (assertBool "listarProp form1" $ sameList ["p"] (listarProp form1))
form2_listarProp_test  = TestCase (assertBool "listarProp form2" $ sameList ["q","r","p"] (listarProp form2))
form3_listarProp_test  = TestCase (assertBool "listarProp form3" $ sameList ["q","r"] (listarProp form3))
form4_listarProp_test  = TestCase (assertBool "listarProp form4" $ sameList ["p","q","r"] (listarProp form4))
form5_listarProp_test  = TestCase (assertBool "listarProp form5" $ sameList ["p","q"] (listarProp form5))


form1_sustCon_test = TestCase (assertEqual "sustCon form1" p (sustCon form1 And Or))
form2_sustCon_test = TestCase (assertEqual "sustCon form2" (Bin (Bin (V "q") Or (V "r")) Imp (V "p")) (sustCon form2 And Or))
form3_sustCon_test = TestCase (assertEqual "sustCon form3" (Bin (Neg (V "q")) And (V "r"))  (sustCon form3 Or And))
form4_sustCon_test = TestCase (assertEqual "sustCon form4" (Bin (Bin (Neg (V "p")) And (Neg (V "q"))) And (V "r"))  (sustCon form4 Or And))
form5_sustCon_test = TestCase (assertEqual "sustCon form5" (Bin (Bin (Bin p Imp q) Imp p) Imp p) (sustCon form5 And Or))


form1_swapCon_test = TestCase (assertEqual "swapCon form1" p (swapCon form1 And Or))
form2_swapCon_test = TestCase (assertEqual "swapCon form2" (Bin (Bin (V "q") Or (V "r")) Imp (V "p")) (swapCon form2 And Or))
form3_swapCon_test = TestCase (assertEqual "swapCon form3" (Bin (Neg (V "q")) And (V "r"))  (swapCon form3 And Or))
form4_swapCon_test = TestCase (assertEqual "swapCon form4" (Bin (Bin (Neg (V "p")) And (Neg (V "q"))) And (V "r")) (swapCon form4 And Or))
form5_swapCon_test = TestCase (assertEqual "swapCon form5" (Bin (Bin (Bin (V "p") Imp (V "q")) Imp (V "p")) Imp (V "p")) (swapCon form5 And Or))

form1_invertir_test  = TestCase (assertEqual "invertir form1" (Neg p) (invertir form1))
form2_invertir_test  = TestCase (assertEqual "invertir form2" (Bin (Bin (Neg (V "q")) Or (Neg (V "r"))) Imp (Neg (V "p"))) (invertir form2))
form3_invertir_test  = TestCase (assertEqual "invertir form3" (Bin (V "q") And (Neg (V "r"))) (invertir form3))
form4_invertir_test  = TestCase (assertEqual "invertir form4" (Bin (Bin (V "p") And (V "q")) And (Neg (V "r"))) (invertir form4))
form5_invertir_test  = TestCase (assertEqual "invertir form5" (Bin (Bin (Bin (Neg (V "p")) Imp (Neg (V "q"))) Imp (Neg (V "p"))) Imp (Neg (V "p"))) (invertir form5))


form1_sustSimp_test  = TestCase (assertEqual "sustSimp form1" (Bin (V "r") Or (V "s")) (sustSimp "p" (Bin (V "r") Or (V "s")) (V "p")))
form2_sustSimp_test  = TestCase (assertEqual "sustSimp form2" (Bin (Bin (V "q") And (V "r")) Imp (Bin(V "r") Or (V "s"))) (sustSimp "p" (Bin (V "r") Or (V "s")) form2))
form3_sustSimp_test  = TestCase (assertEqual "sustSimp form3" (Bin (Neg q) Or r) (sustSimp "p" (Bin (V "r") Or (V "s")) form3))
form4_sustSimp_test  = TestCase (assertEqual "sustSimp form4" (Bin(Bin(Neg(Neg(V "a"))) Or (Neg(V "q"))) Or (V "r")) (sustSimp "p" (Neg(V "a")) form4))
form5_sustSimp_test  = TestCase (assertEqual "sustSimp form5" (Bin (Bin (Bin (V "s") Imp q) Imp (V "s")) Imp (V "s")) (sustSimp "p" (V "s") form5))

form1_sustMult_test  = TestCase (assertEqual "sustMult form1" (Bin(Bin (V "r") Or (V "s")) And (Bin (V "s") Imp (V "t"))) (sustMult [("p", (Bin (V "r") Or (V "s"))), ("q", (Bin (V "s") Imp (V "t")))] (Bin (V "p") And (V "q"))))
form2_sustMult_test  = TestCase (assertEqual "sustMult form2" (Bin(Bin(Bin (V "s") Imp (V "t")) And (V "r")) Imp (Bin (V "r") Or (V "s")))  (sustMult [("p", (Bin (V "r") Or (V "s"))), ("q", (Bin (V "s") Imp (V "t")))] (Bin (Bin q And r) Imp p)))
form3_sustMult_test  = TestCase (assertEqual "sustMult form3" (Bin(Neg(Bin (V "s") Imp (V "t"))) Or (V "r")) (sustMult [("p", (Bin (V "r") Or (V "s"))), ("q", (Bin (V "s") Imp (V "t")))] (Bin (Neg q) Or r)))

cantBinTests = [TestLabel "cantBin 1" form1_cantBin_test, 
                TestLabel "cantBin 2" form2_cantBin_test,
                TestLabel "cantBin 3" form3_cantBin_test,
                TestLabel "cantBin 4" form4_cantBin_test,
                TestLabel "cantBin 5" form5_cantBin_test]

valoresTests = [TestLabel "valores 1" form1_valores_test, 
                TestLabel "valores 2" form2_valores_test,
                TestLabel "valores 3" form3_valores_test,
                TestLabel "valores 4" form4_valores_test,
                TestLabel "valores 5" form5_valores_test]

dobleNegTests = [TestLabel "dobleNeg 1" form11_dobleNeg_test,
                TestLabel "dobleNeg 2" form12_dobleNeg_test,
                TestLabel "dobleNeg 3" form13_dobleNeg_test,
                TestLabel "dobleNeg 4" form14_dobleNeg_test,
                TestLabel "dobleNeg 5" form15_dobleNeg_test]

cambiarTests = [TestLabel "cambiar 1" form1_cambiar_test,
                TestLabel "cambiar 2" form2_cambiar_test,
                TestLabel "cambiar 3" form3_cambiar_test,
                TestLabel "cambiar 4" form4_cambiar_test,
                TestLabel "cambiar 5" form5_cambiar_test]

cantPropXTests = [TestLabel "cantPropX 1" form1_cantPropX_test, 
                  TestLabel "cantPropX 2" form2_cantPropX_test,
                  TestLabel "cantPropX 3" form3_cantPropX_test,
                  TestLabel "cantPropX 4" form4_cantPropX_test,
                  TestLabel "cantPropX 5" form5_cantPropX_test]

listarPropTests = [TestLabel "listarProp 1" form1_listarProp_test, 
                  TestLabel "listarProp 2" form2_listarProp_test,
                  TestLabel "listarProp 3" form3_listarProp_test,
                  TestLabel "listarProp 4" form4_listarProp_test,
                  TestLabel "listarProp 5" form5_listarProp_test]

sustConTests = [TestLabel "sustCon 1" form1_sustCon_test, 
                TestLabel "sustCon 2" form2_sustCon_test,
                TestLabel "sustCon 3" form3_sustCon_test,
                TestLabel "sustCon 4" form4_sustCon_test,
                TestLabel "sustCon 5" form5_sustCon_test]

swapConTests = [TestLabel "swapCon 1" form1_swapCon_test, 
                TestLabel "swapCon 2" form2_swapCon_test,
                TestLabel "swapCon 3" form3_swapCon_test,
                TestLabel "swapCon 4" form4_swapCon_test,
                TestLabel "swapCon 5" form5_swapCon_test]

invertirTests = [TestLabel "invertir 1" form1_invertir_test, 
                TestLabel "invertir 2" form2_invertir_test,
                TestLabel "invertir 3" form3_invertir_test,
                TestLabel "invertir 4" form4_invertir_test,
                TestLabel "invertir 5" form5_invertir_test]

sustSimpTests = [TestLabel "sustSimp 1" form1_sustSimp_test, 
                TestLabel "sustSimp 2" form2_sustSimp_test,
                TestLabel "sustSimp 3" form3_sustSimp_test,
                TestLabel "sustSimp 4" form4_sustSimp_test,
                TestLabel "sustSimp 5" form5_sustSimp_test]

sustMultTests = [TestLabel "sustMult 1" form1_sustMult_test, 
                TestLabel "sustMult 2" form2_sustMult_test,
                TestLabel "sustMult 3" form3_sustMult_test]


-- Todas las pruebas 
allTests = TestList $ cantBinTests ++ cambiarTests ++ valoresTests ++ dobleNegTests  ++ cantPropXTests ++ listarPropTests ++ swapConTests ++ invertirTests ++ sustConTests ++ sustSimpTests ++ sustMultTests 
test0 = TestList $ cantBinTests ++ valoresTests
-- Formulas

form1 = p 
form2 = (Bin (Bin q And r) Imp p) 
form3 = (Bin (Neg q) Or r) 
form4 = (Bin (Bin (Neg p) Or (Neg q)) Or r) 
form5 = (Bin (Bin (Bin p Imp q) Imp p) Imp p) 

-- Formulas con Doble Negacion.

form11 = Neg(Neg p)
form12 = (Bin (Bin (Neg(Neg q)) And r) Imp p) 
form13 = (Bin (Neg(Neg(Neg q))) Or r) 
form14 = (Bin (Bin (Neg p) Or (Neg q)) Or (Neg(Neg r)))
form15 = (Bin (Bin (Bin p Imp q) Imp (Neg (Neg p))) Imp p)

sameList :: (Ord a) => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys