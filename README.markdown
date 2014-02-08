A higher level DSL for writing C in Haskell on top of [language-c](hackage.haskell.org/package/language-c).

As a trivial example

    {-# LANGUAGE OverloadedStrings #-}
    import Language.C.DSL
    test =
       fun [intTy] "test"[int "a", int "b"] $ hblock [
           creturn ("a" + "b")
      ]

Now we could fire up GHCi and run

    >>> pretty test
    int test(int a, int b)
    {
       return a + b;
    }

to see the resulting C.

For more examples consult the examples/ directory.
