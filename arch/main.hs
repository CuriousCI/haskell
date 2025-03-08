module Main (main) where

main = print (test)

test =
  let z = 20
   in let f = \y -> (y + z)
       in let z = 30 in (f 5)

factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial2 n x y = n + x + y

x = 10

-- test = \y -> y + x

-- func x =
--   \y -> y + ((\x -> y) 2) x
-- name x =
--   ( \niggachu ->
--       (y + ((\x -> (y)) (2)))
--   )
-- name x = (\y -> (y + ((\x -> (y)) (2)) x))

-- add a b = a + b
--
-- factorial number =
--   if number == 0
--     then 1
--     else number * factorial (number - 1)

-- case number of
--   0 -> 1
--   n -> n * factorial (n - 1)
