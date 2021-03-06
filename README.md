# Recursive Let Plugin

An experiment to implement something similar to [RecursiveLet proposal][]
using GHC plugins.

[RecursiveLet proposal]: https://github.com/ghc-proposals/ghc-proposals/pull/401

It only works with GHC 8.10, since GHC modules seem to be getting renamed
a lot these days and I didn't want to bother with CPP. PRs welcome.

This is implemented over the course of a day, and not tested. It seems to
work for simple cases, which should catch most typos.

## Example

```haskell
{-# OPTIONS_GHC -fplugin=RecursiveLetPlugin #-}

module P1 where

foo :: Int
foo =
  let even x = if x == 0 then True else odd (x - 1)
      odd x = even (x - 1)
   in 0
```

Outputs:

```
examples/P1.hs:7:7: warning:
    Recursive definition at examples/P1.hs:7:7
      Through: odd
  |
7 |   let even x = if x == 0 then True else odd (x - 1)
  |       ^^^^

examples/P1.hs:8:7: warning:
    Recursive definition at examples/P1.hs:8:7
      Through: even
  |
8 |       odd x = even (x - 1)
  |       ^^^
```

To supress the warning, it introduces a magic function called `recursive` which
silences the warning for given names.

```haskell
foo :: Int
foo =
  let _ = recursive [even, odd]
      even x = if x == 0 then True else odd (x - 1)
      odd x = even (x - 1)
   in 0
```

It works similarly for `where` blocks.

