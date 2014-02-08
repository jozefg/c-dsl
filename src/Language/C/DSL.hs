-- | This module simply exists to export the entirety of the DSL and as
-- a convenience, 'Language.C'.
--
-- A simple example for using this might be something like
-- @
--     {-# LANGUAGE OverloadedStrings #-}
--     import Language.C.DSL
--
--     test =
--        fun [intTy] "test"[int "a", int "b"] $ hblock [
--            creturn ("a" + "b")
--        ]
-- @
-- Now we could fire up GHCi and run
-- @
--     Main*> pretty test
--    int test(int a, int b)
--    {
--       return a + b;
--    }
-- @

module Language.C.DSL (
  module Language.C.DSL.StringLike,
  module Language.C.DSL.Exp,
  module Language.C.DSL.Stat,
  module Language.C.DSL.Decl,
  module Language.C) where

import Language.C
import Language.C.DSL.StringLike
import Language.C.DSL.Exp
import Language.C.DSL.Stat
import Language.C.DSL.Decl
