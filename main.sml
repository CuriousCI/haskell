(* expressions vs statements *)

signature HASKELL =
sig
  (* type Identifier = string *)
  (*Integer of int | Real of real | Character of char *)
  (* value vs datatype *)

  datatype DataType =
    Integer
  | Real
  | Character
  | Alias of DataType
  | NewType of DataType
  | Type of DataType list list (* TODO: constructors, associated types etc... *)


end

structure Haskell :> HASKELL =
struct
  datatype DataType =
    Integer
  | Real
  | Character
  | Alias of DataType
  | NewType of DataType
  | Type of DataType list list (* TODO: constructors, associated types etc... *)

  val list = Type ([] :: (Integer :: []) :: [])
end


(* obiettivo: fattoriale *)
fun factorial 0 = 1
  | factorial n =
      n * (factorial (n - 1));

val y = factorial 4

val x =
  10 (*datatype Language =
     Constant of int
     | Variable of string
     | Plus of Language * Language
     | Let of string * Language * Language
     
     type MyType = bool * int
     
     (* fun blah x : MyType; *)
     fun blah (x: MyType) =
     let val (a, b) = x
     in a
     end
     
     fun eval (Constant n) = n
     | eval (Variable x) = 99
     | eval (Plus (m, n)) =
     (eval n) + (eval m)
     | eval (Let (x, m, n)) = eval n *)
