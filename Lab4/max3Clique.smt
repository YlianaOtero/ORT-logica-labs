; Script template de SMT-LIB para resolver el ejercicio 3.5

(set-logic QF_UF)
(set-option :produce-models true)

(declare-fun p () Bool)
; declarar variables aquí

(define-fun formula () Bool
  ; insertar formalización aquí
)

(assert formula)
(check-sat)
(get-model)
