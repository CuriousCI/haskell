module Main (main) where

main = print (x 1)

x =
  (\y -> (y + ((\x -> y) 2) x))


-- add a b = a + b
--
-- factorial number =
--   if number == 0
--     then 1
--     else number * factorial (number - 1)

-- case number of
--   0 -> 1
--   n -> n * factorial (n - 1)
