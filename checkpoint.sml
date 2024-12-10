datatype ('a, 'b) Result = Ok of 'a | Err of 'b

(* di solito si usa "literal" per indicare un "valore costante" nel codice *)
(* le alternative che avete usato sono Const e K. *)
(* datatype Literal =
  Int of int
| Real of real
| Char of char
| Bool of bool

(* infixr 5 op+ *)

fun add (Int x, Int y) =
      Ok (Int (x + y))
  | add (Int x, Real y) =
      Ok (Real (Real.fromInt x + y))
  | add (Real x, Int y) =
      Ok (Real (x + Real.fromInt y))
  | add (Real x, Real y) =
      Ok (Real (x + y))
  | add (_, _) = Err ""

fun sub (Int x, Int y) =
      Ok (Int (x - y))
  | sub (Int x, Real y) =
      Ok (Real (Real.fromInt x - y))
  | sub (Real x, Int y) =
      Ok (Real (x - Real.fromInt y))
  | sub (Real x, Real y) =
      Ok (Real (x - y))
  | sub (_, _) = Err "" *)

signature TYPE_OP =
sig
  type t
  type u
  val operation: t * t -> u
end

signature OP =
sig
  val opInt: (int * int -> int) option
  val opReal: (real * real -> real) option
  val opChar: (char * char -> char) option
  val opBool: (bool * bool -> bool) option
end

datatype Literal = Int of int | Real of real | Char of char | Bool of bool
datatype Err = MismatchedTypes | OperationNotSupported

functor LiteralOperation(O: OP): TYPE_OP =
struct
  open O
  type t = Literal
  type u = (t, Err) Result

  fun operation (Int x, Int y) =
        (case opInt of
           SOME f => Ok (Int (f (x, y)))
         | _ => Err OperationNotSupported)

    (* | operation (Int x, Real y) =
        (case (O.op_real (Real.fromInt x, y)) of
           (SOME z) => Ok (Real z)
         | _ => Err OperationNotSupported) *)
    (* | operation (Int x, Real y) =
        Ok (Real (O.op_real (Real.fromInt x, y)))
    | operation (Real x, Int y) =
        Ok (Real (O.op_real (x, Real.fromInt y)))
    | operation (Real x, Real y) =
        Ok (Real (O.op_real (x, y)))*)
    | operation (x, y) =
        case (x, y) of
          (Char _, _) => Err OperationNotSupported
        | (Bool _, _) => Err OperationNotSupported
        | (_, Char _) => Err OperationNotSupported
        | (_, Bool _) => Err OperationNotSupported
        | _ => Err MismatchedTypes
end

structure Plus: OP =
struct
  val opInt = SOME op+
  val opReal = SOME (fn (x: real, y) => x + y)
  val opChar = NONE
  val opBool = NONE
end

structure PlusLiteralOperation = LiteralOperation(Plus)


(* structure Literal: TYPE_OP =
struct
  type t = Literal
  datatype Err = MismatchedTypes | OperationNotSupported
  type u = (t, Err) Result

  fun add (Int x, Int y) =
        Ok (Int (x + y))
    | add (Int x, Real y) =
        Ok (Real (Real.fromInt x + y))
    | add (Real x, Int y) =
        Ok (Real (x + Real.fromInt y))
    | add (Real x, Real y) =
        Ok (Real (x + y))
    | add (x, y) =
        case (x, y) of
          (Char _, _) => Err OperationNotSupported
        | (Bool _, _) => Err OperationNotSupported
        | (_, Char _) => Err OperationNotSupported
        | (_, Bool _) => Err OperationNotSupported
        | _ => Err MismatchedTypes
end *)

(* (Char _, _) | (_, Char _) | (Bool _, _) | (_, Bool _) => Err
* OperationNotSupported,*)

(* | add  = Err OperationNotSupported
| add  = Err OperationNotSupported
| add  = Err OperationNotSupported
| add  = Err OperationNotSupported
| add (_, _) = Err MismatchedTypes *)

infix 5 add

open Literal

val x = Int 5 add Int 6

infix +

infix add


(* val result = (Int 5) add (Int 6) *)

(* (case (x, y) of
   (Int x, Int y) => Ok (Int (x + y))
 | _ => Err "")*)


(* di solito si usa "identifier" per indicare il "nome di qualcosa" nel codice *)
type Identifier = string

(* TODO: ANSI ESCAPE CODE per pretty print degli errori *)

(* è più ragionevole chiamarlo Expression: Haskell e HaskellFun non vanno bene 
* perché ci sono cose in Haskell che non sono espressioni, (suppongo i moduli?) *)

(* qui ad esempio usano più tipi di Expression, effettivamente si potrebbe
* provare a fare una distinzione (non che serva più di tanto nel nostro caso) https://tree-sitter.github.io/tree-sitter/creating-parsers#structuring-rules-well *)
datatype Expression =
  Const of Literal
| Var of Identifier (* avete usato tutti Var, mi adeguo anche io *)

| Plus of Expression * Expression
| Times of Expression * Expression
| Minus of Expression * Expression

(* TODO: sarebbe figo mettere la divisione, e le altre "operazioni base" disponibili in Haskell, tipo: *)
(* Unary minus, Absolute value, Floor, Sum, Difference, Product, Division, Int division Remainder, Modulus, Power, Less than, Greater than, Less or equal, Greater or equal, Equal, Not equal *)

| Eq of Expression * Expression
| Lt of Expression * Expression
(* Le altre si possono ricavare da queste due e dai connettivi logici. *)

| And of Expression * Expression
| Or of Expression * Expression
| Not of Expression
(* Ho levato "Implication" da me, e penso di lasciare questi 3. Tecnicamente con
* l'implicazione posiamo fare anche questi? Però credo che inizialmente sarebbe
* più comodo lavorare così *)

| IfThenElse of Expression * Expression * Expression
(* Mi sembra più espressivo metterlo così, anche se io e Francesco abbiamo
* usato entrambi "If". Per me sarebbe uguale. *)

| Let of Identifier * Expression * Expression
(* Penso sia  ragionevole tenere let x = M in N così com'è. *)

| Fn of Identifier * Expression
| Function of Identifier * Identifier * Expression * Expression
(* Alla fine ho deciso di implementarli entrambi per semplicità, al massimo si
* possono levare *)

| Call of Identifier * Expression


(* structure Expression: ADD =
struct
  type T = Expression
  type U = Expression

  fun op+ (x, y) = Plus (x, y)
end
open Expression*)

type 'a Environment = (Identifier * 'a) list

datatype Binding =
  Closure of Identifier * Expression * Binding Environment
| Expression of Expression * Binding Environment

type Closure = Identifier * Expression * Binding Environment

(* questa qui cerca una variabile in un ambiente, in teoria funziona anche
* per il contesto dei tipi, essendo generica *)
fun lookup (key, (entry_key, entry_value) :: entries) =
      if String.compare (key, entry_key) = EQUAL then SOME entry_value
      else lookup (key, entries)
  | lookup (_, []) = NONE


(* la valutazione ritorna un risultato, che può essere "Literal" oppure la
* chiusura di una funzione *)
datatype Value =
  Literal of Literal
| Closure of Closure


(*fun operation (f, x, y) =
  (case (x, y) of
     (Ok (Literal x), Ok (Literal y)) =>
       (case (x, y) of
          (Int x, Int y) => Ok (f (x, y))
        | (Int x, Real y) => Ok (f (Real.fromInt x, y))
        (* | (Int x, Real y) => Ok (Literal (Real (Real.fromInt x + y)))
        | (Real x, Int y) => Ok (Literal (Real (x + Real.fromInt y)))
        | (Real x, Real y) => Ok (Literal (Real (x + y))) *)
        | _ => Err "")
   | _ => Err "")*)

(* nel mio l'avevo chiamata "evaluate", ma forse conviene lasciare "eval" *)
fun eval (Const k, _) =
      Ok (Literal k)
  | eval (Var var, env) =
      (case lookup (var, env) of
         SOME (Expression (exp, exp_env)) => eval (exp, exp_env)
       | _ => Err "variable not declared")

  (*(case (x, y) of
     (Int x, Int y) => Ok (Literal (Int (x + y)))
   | (Int x, Real y) => Ok (Literal (Real (Real.fromInt x + y)))
   | (Real x, Int y) => Ok (Literal (Real (x + Real.fromInt y)))
   | (Real x, Real y) => Ok (Literal (Real (x + y)))
   | _ => Err "")*)

  (* TODO: si possono fare errori più dettagliati per queste operazioni base: 
  * basta considerare tutti i casi  possibili (che sono una marea) 
  * magari si può realizzare una funzione per farlo *)
  (*| eval (Plus (x, y), env) =
      (case (eval (x, env), eval (y, env)) of
         (Ok (Literal x), Ok (Literal y)) => x + y
       | _ => Err "")*)
  | eval (Minus (x, y), env) =
      (case (eval (x, env), eval (y, env)) of
         (Ok (Literal x), Ok (Literal y)) =>
           (case (x, y) of
              (Int x, Int y) => Ok (Literal (Int (x - y)))
            | (Int x, Real y) => Ok (Literal (Real (Real.fromInt x - y)))
            | (Real x, Int y) => Ok (Literal (Real (x - Real.fromInt y)))
            | (Real x, Real y) => Ok (Literal (Real (x - y)))
            | _ => Err "")
       | _ => Err "")
  | eval (Times (x, y), env) =
      (case (eval (x, env), eval (y, env)) of
         (Ok (Literal x), Ok (Literal y)) =>
           (case (x, y) of
              (Int x, Int y) => Ok (Literal (Int (x * y)))
            | (Int x, Real y) => Ok (Literal (Real (Real.fromInt x * y)))
            | (Real x, Int y) => Ok (Literal (Real (x * Real.fromInt y)))
            | (Real x, Real y) => Ok (Literal (Real (x * y)))
            | _ => Err "")
       | _ => Err "")
  (* mi sono perso qualche combinazione di tipo? *)

  (* | eval (Times (m, n), env) =
      (case (eval (m, env), eval (n, env)) of
         (Ok (Literal (Int m)), Ok (Literal (Int n))) =>
           Ok (Literal (Int (m * n)))
       | (Ok (Literal (Real m)), Ok (Literal (Real n))) =>
           Ok (Literal (Real (m * n)))
       | _ => Err "")*)

  | eval (Eq (m, n), env) =
      (case (eval (m, env), eval (n, env)) of
         (Ok (Literal m), Ok (Literal n)) => Ok (Literal (Bool (m = n)))
       (*case (m, n) of
          (Int m, Int n) => Ok (Literal (Bool (Int.compare (m, n) = EQUAL)))
        | (Char m, Char n) =>
            Ok (Literal (Bool (Char.compare (m, n) = EQUAL)))
        | (Bool m, Bool n) => Ok (Literal (Bool (m = n)))
        | _ => Err ""*)
       | _ => Err "")
  | eval (Lt (m, n), env) =
      (case (eval (m, env), eval (n, env)) of
         (Ok (Literal (Int m)), Ok (Literal (Int n))) =>
           Ok (Literal (Bool (Int.compare (m, n) = LESS)))
       | (Ok (Literal (Char m)), Ok (Literal (Char n))) =>
           Ok (Literal (Bool (Char.compare (m, n) = LESS)))
       | _ => Err "")

  | eval (And (m, n), env) =
      (case (eval (m, env), eval (n, env)) of
         (Ok (Literal (Bool m)), Ok (Literal (Bool n))) =>
           Ok (Literal (Bool (m andalso n)))
       | _ => Err "")
  | eval (Or (m, n), env) =
      (case (eval (m, env), eval (n, env)) of
         (Ok (Literal (Bool m)), Ok (Literal (Bool n))) =>
           Ok (Literal (Bool (m orelse n)))
       | _ => Err "")
  | eval (Not m, env) =
      (case eval (m, env) of
         Ok (Literal (Bool m)) => Ok (Literal (Bool (not m)))
       | _ => Err "")

  | eval (IfThenElse (cond, m, n), env) =
      (case eval (cond, env) of
         Ok (Literal (Bool true)) => eval (m, env)
       | Ok (Literal (Bool false)) => eval (n, env)
       | _ => Err "invalid condition")

  | eval (Let (var, m, n), env) =
      eval (n, (var, Expression (m, env)) :: env)
  | eval (Fn (arg, body), env) =
      Ok (Closure (arg, body, env))

  (* | eval (Call (m, n), ) *)


  (* 
  | Fn of Identifier * Expression
  | Function of Identifier * Identifier * Expression * Expression
  | Call of Identifier * Expression *)

  (*| eval (Var var, env) =
      (case lookup (var, env) of
         SOME (BindingExpression (exp, exp_env)) => eval (exp, exp_env)
       | _ => Err "variable not declared") *)
  | eval _ = Err "non funziona, mi disp"

val x =
  0 (* | eval (Variable variable, environment) =
        (case search (variable, environment) of
           SOME (VariableBinding (_, (expression, associated_environment))) =>
             eval (expression, associated_environment)
         | _ => NONE)
    | eval (Implies (a, b), environment) =
        (case (eval (a, environment), eval (b, environment)) of
           (SOME (Bool a), SOME (Bool b)) => SOME (Bool (not a orelse b))
         | _ => NONE)
    | eval (Eq (m, n), environment) =
        (case (eval (m, environment), eval (n, environment)) of
           (SOME (Int m), SOME (Int n)) =>
             SOME (Bool (Int.compare (m, n) = EQUAL))
         | (SOME (Real m), SOME (Real n)) =>
             SOME (Bool (Real.compare (m, n) = EQUAL))
         | (SOME (Char m), SOME (Char n)) =>
             SOME (Bool (Char.compare (m, n) = EQUAL))
         | (SOME (Bool m), SOME (Bool n)) => SOME (Bool (m = n))
         | _ => NONE)
    | eval (If (condition, m, n), environment) =
        (case eval (condition, environment) of
           SOME (Bool true) => eval (m, environment)
         | SOME (Bool false) => eval (n, environment)
         | _ => NONE)
    | eval (Times (m, n), environment) =
        (case (eval (m, environment), eval (n, environment)) of
           (SOME (Int m), SOME (Int n)) => SOME (Int (m * n))
         | (SOME (Real m), SOME (Real n)) => SOME (Real (m * n))
         | _ => NONE)
    | eval (Minus (m, n), environment) =
        (case (eval (m, environment), eval (n, environment)) of
           (SOME (Int m), SOME (Int n)) => SOME (Int (m - n))
         | (SOME (Real m), SOME (Real n)) => SOME (Real (m - n))
         | _ => NONE)
    | eval (Let (variable, expression, scope), environment) =
        eval
          ( scope
          , VariableBinding (variable, (expression, environment)) :: environment
          )
    | eval (Function (function, argument, body, scope), environment) =
        eval
          ( scope
          , FunctionClosure (function, (argument, body, environment))
            :: environment
          )
    | eval (Call (function, expression), environment) =
        (case search (function, environment) of
           SOME (FunctionClosure (_, (argument, body, associated_environment))) =>
             eval
               ( body
               , FunctionClosure
                   (function, (argument, body, associated_environment))
                 :: VariableBinding (argument, (expression, environment))
                 :: associated_environment
               )
         | _ => NONE)
    
    fun debug program =
    print
      ((case eval (program, []) of
          SOME (Int x) => Int.toString x
        | SOME (Bool x) => Bool.toString x
        | _ => "NONE") ^ "\n")
    
    val sum = Let ("x", Literal (Int 5), Times (Variable "x", Times
    (Literal (Int 4), Literal (Int 5))));
    
    debug sum
    
    val simple_function = Function ("simple", "x", Literal (Int 10), Call
    ("simple", Literal (Int 5)));
    
    debug simple_function
    
    val factorial =
    Function
      ( "factorial"
      , "n"
      , If
          ( Eq (Variable "n", Literal (Int 0))
          , Literal (Int 1)
          , Times (Variable "n", Call ("factorial", Minus
              (Variable "n", Literal (Int 1))))
          )
      , Call ("factorial", Literal (Int 4))
      );
    
    debug factorial
    
    val lazy_vs_eager =
    Function ("recursive", "x", Call ("recursive", Variable "x"), Let
      ( "x"
      , Call ("recursive", Literal (Int 1))
      , Literal (Int 10) (*Variable "x"*)
      ));
    
    debug lazy_vs_eager
    
    val static_vs_dynamic =
    Let
      ( "x"
      , Literal (Int 10)
      , Function
          ( "something"
          , "x"
          , Times (Variable "x", Literal (Int 10))
          , Call ("something", Literal (Int 5))
    
          )
      );
    
    debug static_vs_dynamic *)
