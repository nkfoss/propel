(ns propel.default)

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
   'in1
   'exec_dup
   'exec_if
   'boolean_and
   'boolean_or
   'boolean_not
   'boolean_=
   'string_=
   'string_concat
   'string_length
   'string_includes?
   'string_removechar
   'close
   0
   1
   true
   false
   "a"
   "e"
   "i"
   "o"
   "u"
   "A"
   "E"
   "I"
   "O"
   "U"
   ))