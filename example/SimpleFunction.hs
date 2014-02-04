{-# LANGUAGE OverloadedStrings #-}
module SimpleFunction where
import Language.C.DSL

plus =
  fun [intTy] "myPlus"[int "a", int "b"] $ hBlock [
    creturn ("a" + "b")
  ]
minus =
  fun [intTy] "myMinus"[int "a", int "b"] $ hBlock [
    creturn ("a" - "b")
  ]
toplevel = transUnit [export plus, export minus]
