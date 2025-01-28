(* multiple stacks *)

structure Parser =
struct
  datatype Lex =
    Empty
  | Word of string
  | Unknown of string
  | Var of string
  | Fn of string
  | Literal of Haskell.Literal
  | If
  | Let

  datatype State =
    None
  | LPar
  | Num
  | LTick
  | Char
  | T
  | Tr
  | Tru
  | True
  | F
  | Fa
  | Fal
  | False
  | DoneConst
  | Op of char
  | EmptyBeforeBody
  | Lambda
  | LambdaVar
  | LambdaVarDone
  | LambdaTick
  | LambdaArrow
  | LambdaBeforeBody
  | UnknownWord
  | FirstEmptyAfterWord
  | EmptyAfterWord
  | FunctionEqual
  | EmptyAfterFunctionEqual

  (* Read ( -> either read x, \, 2 ->  *)

  fun parse (s, ctx, state) =
    case s of
      #"(" :: l => parse (l, Empty :: ctx, state)
    | a :: l =>
        (case ctx of
           Empty :: res => parse (l, (Word (Char.toString a)) :: res, state)
         | (Word w) :: res =>
             parse (l, Word ((Char.toString a) ^ w) :: res, state)
         (* case a, space, not space*)
         | _ :: res => ctx
         | [] => ctx)
    | [] => ctx

  fun toString lex =
    (case lex of
       Empty :: l => "(" ^ (toString l) ^ ")"
     | (Word word) :: l => "word: " ^ word ^ " (" ^ (toString l) ^ ")"
     | (Unknown unknown) :: l =>
         "unknown: " ^ unknown ^ " (" ^ (toString l) ^ ")"
     | (Var var) :: l => "var: " ^ var ^ " (" ^ (toString l) ^ ")"
     | (Fn f) :: l => "fn " ^ f ^ " => (" ^ (toString l) ^ ")"
     | (Literal _) :: l => "k (" ^ (toString l) ^ ")"
     | _ => "");
end (* let val l1 as (n :: res) = ctx in parse (l, ctx) end *) (* val x = 1; (* type Identifier = string *) (**) (* datatype Program = Lexeme of Lexeme | Whitespace of Whitespace *) (* and Lexeme = *) (*   QVarId *) (* | QConId *) (* | QVarSym *) (* | QConSym *) (* | Literal of Literal *) (* | Special of Special *) (* | ReserverdOp *) (* | ReserverdId of ReservedId *) (* and  Literal = Integer | Float| Char | String *) (* and  Special = LPar | RPar | Comm | Semi | LSqr | RSqr | Tick | LCur | RCur *) (**) (* and Whitespace = Whitestuff of Whitestuff *) (* and Whitestuff = Whitechar of Whitechar | Comment  *) (* and Whitechar = Newline | Vertab | Space | Tab  *) (**) (**) (**) (* and ReservedId = *) (*   Case *) (* | Class *) (* | Data *) (* | Default *) (* | Deriving *) (* | Do *) (* | Else *) (* | Foreign *) (* | If *) (* | Import *) (* | In *) (* | Infix *) (* | InfixL *) (* | InfixR *) (* | Instance *) (* | Let *) (* | Module *) (* | NewType *) (* | Of *) (* | Then *) (* | Type *) (* | Where *) (* | Blank *) (**) (**) (* and CFS = Module | Body  *) (* fun parse(s) = (case of s) *) *)
