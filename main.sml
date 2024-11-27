(* expressions vs statements *)

(* signature HASKELL =
sig
  (* type Identifier = string *)
  (*Integer of int | Real of real | Character of char *)
  (* value vs datatype *)

  datatype DataType =
    Integer
  | Real
  | Character
  | Bool
  | Alias of DataType
  | NewType of DataType
  | Type of DataType list list (* TODO: constructors, associated types etc... *)
end *)

(*structure Haskell :> HASKELL =
struct*)
(* datatype DataType =
  Integer
| Real
| Character
| Bool
| Alias of DataType
| NewType of DataType
| Type of DataType list list (* TODO: constructors, associated types etc... *)
*)

type Identifier = string

datatype Literal =
  Integer of int
| Real of real
| Character of char
| Boolean of bool

datatype Ordering = Less | Equal | Greater

datatype Expression =
  Literal of Literal
| Variable of Identifier
| Implies of Expression * Expression
| Eq of Expression * Expression
| Let of Identifier * Expression * Expression
| If of Expression * Expression * Expression
| Times of Expression * Expression
| Minus of Expression * Expression
| Function of Identifier * Identifier * Expression * Expression
| Call of Identifier * Expression


(* possible solution *)
datatype Entry =
  FunctionClosure of Identifier * (Identifier * Expression * (Entry list))
| VariableBinding of Identifier * (Expression * (Entry list))

(* search in env *)

(*val integer_list = 1 :: 2 :: 3 :: 5 :: []*)

val y = let val x = 10 in x end

val error = Integer 1

fun search (identifier, FunctionClosure (function, closure) :: environment) =
      if String.compare (identifier, function) = EQUAL then
        SOME (FunctionClosure (function, closure))
      else
        search (identifier, environment)
  | search (identifier, VariableBinding (variable, assignment) :: environment) =
      if String.compare (identifier, variable) = EQUAL then
        SOME (VariableBinding (variable, assignment))
      else
        search (identifier, environment)
  | search (_, []) = NONE

(*let val FunctionClosure (var, exp, env) = search (x, env)
in (eval (exp, env))
end *)

(* Int.compare to compare *)


fun evaluate (Literal literal, _) = SOME literal
  | evaluate (Variable variable, environment) =
      (case search (variable, environment) of
         SOME (VariableBinding (_, (expression, associated_environment))) =>
           evaluate (expression, associated_environment)
       | _ => NONE)
  | evaluate (Implies (a, b), environment) =
      (case (evaluate (a, environment), evaluate (b, environment)) of
         (SOME (Boolean a), SOME (Boolean b)) => SOME (Boolean (not a orelse b))
       | _ => NONE)
  | evaluate (Eq (m, n), environment) =
      (case (evaluate (m, environment), evaluate (n, environment)) of
         (SOME (Integer m), SOME (Integer n)) =>
           SOME (Boolean (Int.compare (m, n) = EQUAL))
       | (SOME (Real m), SOME (Real n)) =>
           SOME (Boolean (Real.compare (m, n) = EQUAL))
       | (SOME (Character m), SOME (Character n)) =>
           SOME (Boolean (Char.compare (m, n) = EQUAL))
       | (SOME (Boolean m), SOME (Boolean n)) => SOME (Boolean (m = n))
       | _ => NONE)
  | evaluate (If (condition, m, n), environment) =
      (case evaluate (condition, environment) of
         SOME (Boolean true) => evaluate (m, environment)
       | SOME (Boolean false) => evaluate (n, environment)
       | _ => NONE)
  | evaluate (Times (m, n), environment) =
      (case (evaluate (m, environment), evaluate (n, environment)) of
         (SOME (Integer m), SOME (Integer n)) => SOME (Integer (m * n))
       | (SOME (Real m), SOME (Real n)) => SOME (Real (m * n))
       | _ => NONE)
  | evaluate (Minus (m, n), environment) =
      (case (evaluate (m, environment), evaluate (n, environment)) of
         (SOME (Integer m), SOME (Integer n)) => SOME (Integer (m - n))
       | (SOME (Real m), SOME (Real n)) => SOME (Real (m - n))
       | _ => NONE)
  | evaluate (Let (variable, expression, scope), environment) =
      evaluate
        ( scope
        , VariableBinding (variable, (expression, environment)) :: environment
        )
  | evaluate (Function (function, argument, body, scope), environment) =
      evaluate
        ( scope
        , FunctionClosure (function, (argument, body, environment))
          :: environment
        )
  | evaluate (Call (function, expression), environment) =
      (case search (function, environment) of
         SOME (FunctionClosure (_, (argument, body, associated_environment))) =>
           evaluate
             ( body
             , FunctionClosure
                 (function, (argument, body, associated_environment))
               :: VariableBinding (argument, (expression, environment))
               :: associated_environment
             )
       | _ => NONE)

val sum = Let ("x", Literal (Integer 5), Times (Variable "x", Times
  (Literal (Integer 4), Literal (Integer 5))));

print
  ((case evaluate (sum, []) of
      SOME (Integer x) => Int.toString x
    | SOME (Boolean x) => Bool.toString x
    | _ => "Eh, volevi! Guarda che faccia!") ^ "\n");

val simple_function = Function ("simple", "x", Literal (Integer 10), Call
  ("simple", Literal (Integer 5)));

print
  ((case evaluate (simple_function, []) of
      SOME (Integer x) => Int.toString x
    | SOME (Boolean x) => Bool.toString x
    | _ => "Eh, volevi! Guarda che faccia!") ^ "\n");

val factorial =
  Function
    ( "factorial"
    , "n"
    , If
        ( Eq (Variable "n", Literal (Integer 0))
        , Literal (Integer 1)
        , Times (Variable "n", Call ("factorial", Minus
            (Variable "n", Literal (Integer 1))))
        )
    , Call ("factorial", Literal (Integer 4))
    );

print
  ((case evaluate (factorial, []) of
      SOME (Integer x) => "factorial " ^ Int.toString x
    | SOME (Boolean x) => Bool.toString x
    | _ => "Eh, volevi! Guarda che faccia!") ^ "\n");

val lazy_vs_eager =
  Function ("recursive", "x", Call ("recursive", Variable "x"), Let
    ("x", Call ("recursive", Literal (Integer 1)), Variable "y"));

print
  ((case evaluate (factorial, []) of
      SOME (Integer x) => "lazy vs eager " ^ Int.toString x
    | SOME (Boolean x) => Bool.toString x
    | _ => "Eh, volevi! Guarda che faccia!") ^ "\n");

val static_vs_dynamic = ()

(* (case evalutate (expression, environment) of
   SOME literal => evalutate ( body , VariableBinding (argument, Literal literal) :: associated_environment)
 | _ => NONE) *)
(*(SOME FunctionClosure(_, (x, body, ass_env))) => (
    case evalutate (m, env) of 
        SOME exp => evalutate(body, VariableBinding(x, exp) :: ass_env)  
       | _ => NONE
)*)

(* evalutate (m, env) *)

(* val part = 1 :: 2 :: 3 :: []
val part2 = 5 :: 6 :: 8 :: part
val part3 = 9 :: 5 :: 2 :: part *)


(* | Times of Expression * Expression
| Function of Identifier * Identifier * Expression * Expression
| Call of Identifier * Expression *)


(* print ("output: " ^ Bool.toString (evalutate_bool True) ^ "\n"); *)
(*      | (Real m, Real n) => Boolean (m = n) *)
(* fun evalutate_bool True = true
  | evalutate_bool False = false
  | evalutate_bool (Implies (a, b)) =
      not (evalutate_bool a) orelse (evalutate_bool b)
  | evalutate_bool (Eq (m, n)) =
      (case ((evalutate_val m), (evalutate_val n)) of
         (Literal (Integer a), Literal (Integer b)) => a = b
       | _ => false)
  | evalutate_bool (Lt (m, n)) =
      (case ((evalutate_val m), (evalutate_val n)) of
         (Literal (Integer a), Literal (Integer b)) => a < b
       | _ => false); *)


val x =
  0 (* val list = Type ([] :: (Integer :: []) :: []) *) (* end *) (* datatype 'a List = Empty | Cons of 'a * 'a List *) (* equals semantic *) (* type Variable = Identifier *)
    (*datatype BooleanExpression =
        True
      | False
      | Implies of BooleanExpression * BooleanExpression
      | Eq of Expression * Expression *) (* | Plus of Expression * Expression *) (* fun Not b = Implies (b, False) *)
    (* datatype Statement =
        Function of Identifier * Identifier * Expression * Expression *)
    (* datatype Haskell =
      Statement of Statement
    | Expression of Expression *) (*print ("output: " ^ Bool.toString (evalutate_bool True) ^ "\n");*) (* Equals con le funzioni *) (* | PartialEq of Value * Value *) (* | PartialOrd of Value * Value *) (*| Case of Identifier * ((Value * Expression) list) * Expression *)
    (* val factorial = Function ("n", Case ("n", [], Times (Variable "n", Call 
      ("factorial", Minus (Variable "n", Constant 1))))) *) (* fun evalutate Statement( Function (identifier, argument, expression)) *) (*functions, types, classes, instances, and other modules.*) (* obiettivo: fattoriale *)
    (* fun factorial 0 = 1
      | factorial n =
          n * (factorial (n - 1)); *)
    (*val x = 10 datatype Language =
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
         
         fun evalutate (Constant n) = n
         | evalutate (Variable x) = 99
         | evalutate (Plus (m, n)) =
         (evalutate n) + (evalutate m)
         | evalutate (Let (x, m, n)) = evalutate n *)
