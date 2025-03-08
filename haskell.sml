structure Haskell =
struct
  type Var = string

  datatype Exp =
    Literal of Literal
  | Var of Var
  | IfThenElse of Exp * Exp * Exp
  | Let of Var * Exp * Exp
  | Call of Exp * Exp
  | Fun of Var * Var * Exp * Exp
  | Op of Op
  and Literal =
    Int of int
  | Real of real
  | Char of char
  | Bool of bool
  | Unit
  | Fn of Var * Exp
  and Op =
    And of Exp * Exp
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

  fun debug (SOME (Int l)) = Int.toString l
    | debug (SOME (Real l)) = Real.toString l
    | debug (SOME (Bool l)) = Bool.toString l
    | debug (SOME (Char l)) = Char.toString l
    | debug (SOME Unit) = "()"
    | debug (SOME (Fn (x, exp))) =
        "\\" ^ x ^ " -> " ^ toString exp
    | debug NONE = "none"
  and toString (Literal l) =
        debug (SOME l)
    | toString (Var var) = var
    | toString (IfThenElse (exp, exp', exp'')) =
        "if " ^ toString exp ^ " then " ^ toString exp' ^ " else "
        ^ toString exp''
    | toString (Let (var, exp, exp')) =
        "let " ^ var ^ " = " ^ toString exp ^ " in " ^ toString exp'
    | toString (Call (exp, exp')) =
        "(" ^ toString exp ^ " " ^ toString exp' ^ ")"
    | toString (Fun (name, var, exp, exp')) =
        name ^ " " ^ var ^ " = " ^ toString exp ^ " in " ^ toString exp'
    | toString (Op (And (exp, exp'))) =
        "(" ^ toString exp ^ " && " ^ toString exp' ^ ")"
    | toString (Op (Or (exp, exp'))) =
        "(" ^ toString exp ^ " || " ^ toString exp' ^ ")"
    | toString (Op (Lt (exp, exp'))) =
        "(" ^ toString exp ^ " < " ^ toString exp' ^ ")"
    | toString (Op (Leq (exp, exp'))) =
        "(" ^ toString exp ^ " <= " ^ toString exp' ^ ")"
    | toString (Op (Gt (exp, exp'))) =
        "(" ^ toString exp ^ " > " ^ toString exp' ^ ")"
    | toString (Op (Geq (exp, exp'))) =
        "(" ^ toString exp ^ " >= " ^ toString exp' ^ ")"
    | toString (Op (Eq (exp, exp'))) =
        "(" ^ toString exp ^ " == " ^ toString exp' ^ ")"
    | toString (Op (Add (exp, exp'))) =
        "(" ^ toString exp ^ " + " ^ toString exp' ^ ")"
    | toString (Op (Sub (exp, exp'))) =
        "(" ^ toString exp ^ " - " ^ toString exp' ^ ")"
    | toString (Op (Mul (exp, exp'))) =
        "(" ^ toString exp ^ " * " ^ toString exp' ^ ")"
    | toString (Op (Div (exp, exp'))) =
        "(" ^ toString exp ^ " / " ^ toString exp' ^ ")"
    | toString (Op (Pow (exp, exp'))) =
        "(" ^ toString exp ^ " ^ " ^ toString exp' ^ ")"

  datatype Entry = Entry of Var * (Exp * (Entry list))
  datatype Store = Store of Var * (Literal * (Store list))

  (* datatype Entry = *)
  (*   FunctionClosure of Identifier * (Identifier * Expression * (Entry list)) *)
  (* | VariableBinding of Identifier * (Expression * (Entry list)) *)

  fun lookup (key, (Entry (k, v) :: entries)) =
        if String.compare (key, k) = EQUAL then SOME v
        else lookup (key, entries)
    | lookup (_, []) = NONE

  (* fun lookup (key, (entry_key, entry_val) :: entries) = *)
  (*       if String.compare (key, entry_key) = EQUAL then SOME entry_val *)
  (*       else lookup (key, entries) *)
  (*   | lookup (_, []) = NONE *)

  fun eval (Op op+, env: Entry list) =
        (case op+ of
           And (m, n) =>
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
    | eval (Literal l, env) = SOME l
    | eval (Var x, env) =
        (case lookup (x, env) of
           SOME (exp, env') => eval (exp, env')
         | _ => raise Fail (x ^ " not found"))
    | eval (IfThenElse (c, exp, exp'), env) =
        (case eval (c, env) of
           SOME (Bool true) => eval (exp, env)
         | SOME (Bool false) => eval (exp', env)
         | _ => NONE)
    | eval (Let (x, exp, exp'), env) =
        eval (exp', Entry (x, (exp, env)) :: env)
    | eval (Fun (n, x, exp, exp'), env) =
        eval (exp', Entry (n, (Literal (Fn (x, exp)), env)) :: env)
    | eval (Call (exp, exp'), env) =
        (case eval (exp, env) of
           SOME (Fn (x, exp'')) => eval (exp'', Entry (x, (exp', env)) :: env)
         | _ => NONE)
end

(* val three = fn x => fn y => x (x (x y)) *)
(* fun nat z = *)
(*   (z (fn x => x + 1)) 0 *)

(* fun toString (Literal l) = *)
(*       (case l of *)
(*          Char c => "'" ^ Char.toString c ^ "'" *)
(*        | Int i => "int(" ^ Int.toString i ^ ")" *)
(*        | Real r => "real(" ^ Real.toString r ^ ")" *)
(*        | Bool b => "bool(" ^ Bool.toString b ^ ")") *)
(*   | toString (Var x) = "Var(" ^ x ^ ")" *)
(*   | toString (IfThenElse (a, b, c)) = *)
(*       "if(" ^ toString a ^ ")then(" ^ toString b ^ ")else(" ^ toString c ^ ")" *)
(*   | toString (Fun (name, x, body)) = *)
(*       "fun " ^ name ^ " " ^ x ^ " = (" ^ toString body ^ ")" *)
(*   | toString (Fn (x, exp)) = *)
(*       "fn (" ^ x ^ ") = (" ^ toString exp ^ ")" *)
(*   (* | toString (Op _) = "(stuff ~ stuff)" *) *)
(*   | toString (Let (x, exp1, exp2)) = *)
(*       "let " ^ x ^ " = (" ^ toString exp1 ^ ") in (" ^ toString exp2 ^ ")" *)
(*   | toString (Op z) = *)
(*       (case z of *)
(*          And (n, m) => toString n ^ " And " ^ toString m *)
(*        | Or (n, m) => toString n ^ " Or  " ^ toString m *)
(*        | Lt (n, m) => toString n ^ " Lt  " ^ toString m *)
(*        | Leq (n, m) => toString n ^ " Leq  " ^ toString m *)
(*        | Gt (n, m) => toString n ^ " Gt  " ^ toString m *)
(*        | Geq (n, m) => toString n ^ " Geq  " ^ toString m *)
(*        | Eq (n, m) => toString n ^ " Eq  " ^ toString m *)
(*        | Add (n, m) => toString n ^ " Add  " ^ toString m *)
(*        | Sub (n, m) => toString n ^ " Sub  " ^ toString m *)
(*        | Mul (n, m) => toString n ^ " Mul  " ^ toString m *)
(*        | Div (n, m) => toString n ^ " Div  " ^ toString m *)
(*        | Pow (n, m) => toString n ^ " Pow  " ^ toString m) *)
(*   | toString (Call (n, m)) = *)
(*       "call(" ^ toString n ^ ", " ^ toString m ^ ")" *)
(*   | toString Nop = "nop" *)
val x = 0;
