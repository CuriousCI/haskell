module Main (main) where

main = print (factorial 4)

factorial number =
  if number == 0
    then 1
    else number * factorial (number - 1)

-- case number of
--   0 -> 1
--   n -> n * factorial (n - 1)
