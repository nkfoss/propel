(ns propel.default)

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'integer_=
   'exec_dup
   'exec_if
   'boolean_and
   'boolean_or
   'boolean_not
   'boolean_=
   'string_=
   'string_take
   'string_drop
   'string_reverse
   'string_concat
   'string_length
   'string_includes?
   'close
   0
   1
   true
   false
   ""
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "A"
   "C"
   "G"
   "T"))