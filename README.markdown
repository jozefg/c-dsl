A higher level DSL for writing C in Haskell on top of [language-c](http://hackage.haskell.org/package/language-c).

As a trivial example
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Language.C.DSL
test =
  fun [intTy] "test"[int "a", int "b"] $ hblock [
    creturn ("a" + "b")
  ]
```

Now we could fire up GHCi and run `pretty test`
```c
int test(int a, int b)
{
  return a + b;
}
```
to see the resulting C.

For more examples consult the examples/ directory.
