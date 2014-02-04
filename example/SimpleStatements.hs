{-# LANGUAGE OverloadedStrings #-}
module SimpleStatements where
import Language.C.DSL

whileLoop =
  while ("i" <=: 10) $ block [
    intoB $ int "j" .= 1,
    intoB $ "i" <-- "j" + "i",
    intoB $ "printf"#[str "%d", "i"],
    intoB $ PlusPlus `pre` "i"
  ]
forLoop =
  for(int "i" .= 0, "i" <=: 10, PlusPlus `pre` "i") $ hBlock [
    "printf"#[str"%d", "i"]
  ]
