module Main (main) where

factorial number =
  case number of
    0 -> 1
    n -> n * factorial (n - 1)

main = print (factorial 4)
