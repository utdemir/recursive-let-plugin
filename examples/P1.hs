{-# OPTIONS_GHC -fplugin=RecursiveLetPlugin #-}

module P1 where

foo :: Int
foo =
  let _ = recursive [even, odd]
      even x = if x == 0 then True else odd (x - 1)
      odd x = even (x - 1)
   in 0
