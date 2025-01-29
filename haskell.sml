structure Haskell =
struct
  type Var = string

  datatype Exp =
    Literal of Literal
  | Var of Var
  | IfThenElse of Exp * Exp
  | Let of Var * Exp * Exp
  | Call of Var * Exp list
  | Fn of Var * Exp
  | Fun of Var * Var * Exp
  | Op of Op
  and Literal =
    Int of int
  | Real of real
  | Char of char
  | Bool of bool
  and Op =
    Not of Exp
  | And of Exp * Exp
  | Or of Exp * Exp
  | Lt of Exp * Exp
  | Leq of Exp * Exp
  | Gt of Exp * Exp
  | Geq of Exp * Exp
  | Eq of Exp * Exp
  | Add of Exp * Exp
  | Sub of Exp * Exp
  | Mul of Exp * Exp
  | Div of Exp * Exp
  | Pow of Exp * Exp

  (* | Fun of Var * Var list * Exp * Exp *)
  (* | Case of Exp *)

  fun lookup (key, (entry_key, entry_value) :: entries) =
        if String.compare (key, entry_key) = EQUAL then SOME entry_value
        else lookup (key, entries)
    | lookup (_, []) = NONE

  val x = 2; (* | Tuple of Literal list | String | List | Maybe | Lambda *)

  fun eval (Op op+, env) =
        (case op+ of
           Not m =>
             (case eval (m, env) of
                SOME (Bool m) => SOME (Bool (not m))
              | _ => NONE)
         | And (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Bool m), SOME (Bool n)) => SOME (Bool (m andalso n))
              | _ => NONE)
         | Or (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Bool m), SOME (Bool n)) => SOME (Bool (m orelse n))
              | _ => NONE)
         | Lt (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Bool (m < n))
              | (SOME (Real m), SOME (Real n)) => SOME (Bool (m < n))
              | _ => NONE)
         | Leq (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Bool (m <= n))
              | (SOME (Real m), SOME (Real n)) => SOME (Bool (m <= n))
              | _ => NONE)
         | Gt (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Bool (m > n))
              | (SOME (Real m), SOME (Real n)) => SOME (Bool (m > n))
              | _ => NONE)
         | Geq (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Bool (m >= n))
              | (SOME (Real m), SOME (Real n)) => SOME (Bool (m >= n))
              | _ => NONE)
         | Eq (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) =>
                  SOME (Bool (Int.compare (m, n) = EQUAL))
              | (SOME (Real m), SOME (Real n)) =>
                  SOME (Bool (Real.compare (m, n) = EQUAL))
              | (SOME (Char m), SOME (Char n)) =>
                  SOME (Bool (Char.compare (m, n) = EQUAL))
              | (SOME (Bool m), SOME (Bool n)) => SOME (Bool (m = n))
              | _ => NONE)
         | Add (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Int (Int.+ (m, n)))
              | (SOME (Int m), SOME (Real n)) =>
                  SOME (Real (Real.+ (Real.fromInt m, n)))
              | (SOME (Real m), SOME (Int n)) =>
                  SOME (Real (Real.+ (m, Real.fromInt n)))
              | (SOME (Real m), SOME (Real n)) => SOME (Real (Real.+ (m, n)))
              | _ => NONE)
         | Sub (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Int (Int.- (m, n)))
              | (SOME (Int m), SOME (Real n)) =>
                  SOME (Real (Real.- (Real.fromInt m, n)))
              | (SOME (Real m), SOME (Int n)) =>
                  SOME (Real (Real.- (m, Real.fromInt n)))
              | (SOME (Real m), SOME (Real n)) => SOME (Real (Real.- (m, n)))
              | _ => NONE)
         | Mul (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Int (Int.* (m, n)))
              | (SOME (Int m), SOME (Real n)) =>
                  SOME (Real (Real.* (Real.fromInt m, n)))
              | (SOME (Real m), SOME (Int n)) =>
                  SOME (Real (Real.* (m, Real.fromInt n)))
              | (SOME (Real m), SOME (Real n)) => SOME (Real (Real.* (m, n)))
              | _ => NONE)
         | Div (m, n) =>
             (case (eval (m, env), eval (n, env)) of
                (SOME (Int m), SOME (Int n)) => SOME (Int (Int.div (m, n)))
              | (SOME (Int m), SOME (Real n)) =>
                  SOME (Real (Real./ (Real.fromInt m, n)))
              | (SOME (Real m), SOME (Int n)) =>
                  SOME (Real (Real./ (m, Real.fromInt n)))
              | (SOME (Real m), SOME (Real n)) => SOME (Real (Real./ (m, n)))
              | _ => NONE)
         | Pow (_, _) => NONE)
    | eval (Literal l, _) = SOME l
    | eval (_, _) = NONE
end (* val x = 0; (* (case o ) *) (* fun eval(Not(a)) = if eval *) (* fun eval(a) =  *) (* case Not(a) *) *)
