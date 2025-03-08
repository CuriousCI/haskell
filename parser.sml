structure Parser =
struct
  (* TODO: add types (data, type, newtype) *)
  (* TODO: add monads *)
  (* TODO: case ... of *)
  (* TODO: class *)
  (* TODO: module *)
  (* TODO: pattern-matching *)
  (* TODO: negative numbers -x, (1.0, 1e10) floating, (0o1, 0O1) octal, (0x1, 0X1) hexadecimal *)

  datatype Token =
    LPar
  | RPar
  | Arrow
  | Var of string
  | Char of char
  | Int of int
  | Real of real
  | Bool of bool

  | And
  | Or
  | Leq
  | Lt
  | Geq
  | Gt
  | EqOp
  | Add
  | Sub
  | Mul
  | Div
  | Pow

  | Eq
  | Backslash

  | Case
  | Class
  | Data
  | Deriving
  | Do
  | Else
  | If
  | Import
  | In
  | Instance
  | Let
  | Of
  | Module
  | Newtype
  | Then
  | Type
  | Where

  fun debug LPar = "("
    | debug RPar = ")"
    | debug Arrow = "->"
    | debug (Var var) = var
    | debug (Char l) = Char.toString l
    | debug (Int l) = Int.toString l
    | debug (Real l) = Real.toString l
    | debug (Bool l) = Bool.toString l
    | debug And = "&&"
    | debug Or = "||"
    | debug Leq = "<="
    | debug Lt = "<"
    | debug Geq = ">="
    | debug Gt = ">"
    | debug EqOp = "=="
    | debug Add = "+"
    | debug Sub = "-"
    | debug Mul = "*"
    | debug Div = "/"
    | debug Pow = "^"
    | debug Eq = "="
    | debug Backslash = "\\"
    | debug Case = "case"
    | debug Class = "class"
    | debug Data = "data"
    | debug Deriving = "deriving"
    | debug Do = "do"
    | debug Else = "else"
    | debug If = "if"
    | debug Import = "import"
    | debug In = "in"
    | debug Instance = "instance"
    | debug Let = "let"
    | debug Of = "of"
    | debug Module = "module"
    | debug Newtype = "newtype"
    | debug Then = "then"
    | debug Type = "type"
    | debug Where = "where"

  fun toString (a :: s) =
        debug a ^ " " ^ toString s
    | toString [] = ""

  datatype Literal = Word of string | IntSeq of string | RealSeq of string

  fun fetch (Word w, s) =
        (case s of
           a :: cs =>
             if Char.isAlpha a then fetch (Word (w ^ Char.toString a), cs)
             else (Word w, s)
         | _ => (Word w, s))
    | fetch (IntSeq i, s) =
        (case s of
           #"." :: cs => fetch (RealSeq (i ^ "."), cs)
         | a :: cs =>
             if Char.isDigit a then fetch (IntSeq (i ^ Char.toString a), cs)
             else (IntSeq i, s)
         | _ => (IntSeq i, s))
    | fetch (RealSeq r, s) =
        (case s of
           a :: cs =>
             if Char.isDigit a then fetch (RealSeq (r ^ Char.toString a), cs)
             else (RealSeq r, s)
         | _ => (RealSeq r, s))

  fun s_comm (#"\n" :: s) = s
    | s_comm (_ :: s) = s_comm s
    | s_comm [] = []

  fun m_comm (#"-" :: #"}" :: s) = s
    | m_comm (_ :: s) = m_comm s
    | m_comm [] = []

  fun tokenize [] = []
    | tokenize (#"(" :: s) = LPar :: tokenize s
    | tokenize (#")" :: s) = RPar :: tokenize s
    | tokenize (#"-" :: #"-" :: s) =
        tokenize (s_comm s)
    | tokenize (#"{" :: #"-" :: s) =
        tokenize (m_comm s)
    | tokenize (#"-" :: #">" :: s) = Arrow :: tokenize s
    | tokenize (#"&" :: #"&" :: s) = And :: tokenize s
    | tokenize (#"|" :: #"|" :: s) = Or :: tokenize s
    | tokenize (#"<" :: #"=" :: s) = Leq :: tokenize s
    | tokenize (#"<" :: s) = Lt :: tokenize s
    | tokenize (#">" :: #"=" :: s) = Geq :: tokenize s
    | tokenize (#">" :: s) = Gt :: tokenize s
    | tokenize (#"=" :: #"=" :: s) = EqOp :: tokenize s
    | tokenize (#"+" :: s) = Add :: tokenize s
    | tokenize (#"-" :: s) = Sub :: tokenize s
    | tokenize (#"*" :: s) = Mul :: tokenize s
    | tokenize (#"/" :: s) = Div :: tokenize s
    | tokenize (#"^" :: s) = Pow :: tokenize s
    | tokenize (#"=" :: s) = Eq :: tokenize s
    | tokenize (#"\\" :: s) = Backslash :: tokenize s
    | tokenize (#"T" :: #"r" :: #"u" :: #"e" :: s) = Bool true :: tokenize s
    | tokenize (#"F" :: #"a" :: #"l" :: #"s" :: #"e" :: s) =
        Bool false :: tokenize s
    | tokenize (#"'" :: c :: #"'" :: s) = Char c :: tokenize s

    | tokenize (#"c" :: #"a" :: #"s" :: #"e" :: s) = Case :: tokenize s
    | tokenize (#"c" :: #"l" :: #"a" :: #"s" :: #"s" :: s) = Class :: tokenize s
    | tokenize (#"d" :: #"a" :: #"t" :: #"a" :: s) = Data :: tokenize s
    | tokenize
        (#"d" :: #"e" :: #"r" :: #"i" :: #"v" :: #"i" :: #"n" :: #"g" :: s) =
        Deriving :: tokenize s
    | tokenize (#"d" :: #"o" :: s) = Do :: tokenize s
    | tokenize (#"e" :: #"l" :: #"s" :: #"e" :: s) = Else :: tokenize s
    | tokenize (#"i" :: #"f" :: s) = If :: tokenize s
    | tokenize (#"i" :: #"m" :: #"p" :: #"o" :: #"r" :: #"t" :: s) =
        Import :: tokenize s
    | tokenize
        (#"i" ::
           #"n" :: #"s" :: #"t" :: #"a" :: #"n" :: #"c" :: #"e" :: #"s" :: s) =
        Instance :: tokenize s
    | tokenize (#"i" :: #"n" :: s) = In :: tokenize s
    | tokenize (#"l" :: #"e" :: #"t" :: s) = Let :: tokenize s
    | tokenize (#"o" :: #"f" :: s) = Of :: tokenize s
    | tokenize (#"m" :: #"o" :: #"d" :: #"u" :: #"l" :: #"e" :: s) =
        Module :: tokenize s
    | tokenize (#"n" :: #"e" :: #"w" :: #"t" :: #"y" :: #"p" :: #"e" :: s) =
        Newtype :: tokenize s
    | tokenize (#"t" :: #"h" :: #"e" :: #"n" :: s) = Then :: tokenize s
    | tokenize (#"t" :: #"y" :: #"p" :: #"e" :: s) = Type :: tokenize s
    | tokenize (#"w" :: #"h" :: #"e" :: #"r" :: #"e" :: s) = Where :: tokenize s

    | tokenize (c :: s) =
        if Char.isSpace c then
          tokenize s
        else
          (if Char.isDigit c then
             let
               val (l, cs) = fetch (IntSeq (Char.toString c), s)
             in
               (case l of
                  Word w => Var w :: tokenize cs
                | IntSeq i =>
                    (case Int.fromString i of
                       SOME i => Int i :: tokenize cs
                     | NONE => raise Fail "Int.fromString")
                | RealSeq r =>
                    (case Real.fromString r of
                       SOME r => Real r :: tokenize cs
                     | NONE => raise Fail "Real.fromString"))
             end
           else
             (if Char.isAlpha c then
                let
                  val (l, cs) = fetch (Word (Char.toString c), s)
                in
                  (case l of
                     Word w => Var w :: tokenize cs
                   | IntSeq i =>
                       (case Int.fromString i of
                          SOME i => Int i :: tokenize cs
                        | NONE => raise Fail "Int.fromString")
                   | RealSeq r =>
                       (case Real.fromString r of
                          SOME r => Real r :: tokenize cs
                        | NONE => raise Fail "Real.fromString"))
                end
              else
                raise Fail "tokenization error"))

  fun parse (LPar :: Var name :: Var x :: Eq :: s) =
        (case parse s of
           (exp, RPar :: s') =>
             let val (exp', s'') = parse s'
             in (Haskell.Fun (name, x, exp, exp'), s'')
             end
         | _ =>
             raise Fail
               ("expected ')' when parsing (" ^ name ^ " " ^ x ^ " = ..."))
    | parse (Var name :: Var x :: Eq :: s) =
        let
          val (exp, s') = parse s
        in
          let val (exp', s'') = parse s'
          in (Haskell.Fun (name, x, exp, exp'), s'')
          end
        end
    | parse (LPar :: Backslash :: Var x :: Arrow :: s) =
        (case parse s of
           (exp, RPar :: s') => (Haskell.Literal (Haskell.Fn (x, exp)), s')
         | _ => raise Fail ("expected ')' when parsing (\\" ^ x ^ " -> ..."))
    | parse (Backslash :: Var x :: Arrow :: s) =
        let val (exp, s') = parse s
        in (Haskell.Literal (Haskell.Fn (x, exp)), s')
        end
    | parse (LPar :: If :: s) =
        (case parse s of
           (cond, Then :: s') =>
             (case parse s' of
                (bif, Else :: s'') =>
                  (case parse s'' of
                     (belse, RPar :: s''') =>
                       (Haskell.IfThenElse (cond, bif, belse), s''')
                   | _ =>
                       raise Fail "expected ')' after '(if ... then ... else' ")
              | _ => raise Fail "missing 'else' after 'then'")
         | _ => raise Fail "missing 'then' after 'if ...'")
    | parse (If :: s) =
        (case parse s of
           (cond, Then :: s') =>
             (case parse s' of
                (bif, Else :: s'') =>
                  let val (belse, s''') = parse s''
                  in (Haskell.IfThenElse (cond, bif, belse), s''')
                  end
              | _ => raise Fail "missing 'else' after 'then'")
         | _ => raise Fail "missing 'then' after 'if ...'")
    | parse (LPar :: Let :: Var x :: Eq :: s') =
        (case parse s' of
           (exp, In :: s'') =>
             (case parse s'' of
                (body, RPar :: s''') => (Haskell.Let (x, exp, body), s''')
              | _ =>
                  raise Fail ("missing ')' after 'let " ^ x ^ " = ... in ..."))
         | _ => raise Fail ("missing 'in' after 'let " ^ x ^ " = ...'"))
    | parse (Let :: Var x :: Eq :: s') =
        (case parse s' of
           (exp, In :: s'') =>
             let val (body, s''') = parse s''
             in (Haskell.Let (x, exp, body), s''')
             end
         | _ => raise Fail ("missing 'in' after 'let " ^ x ^ " = ...'"))
    | parse (LPar :: Var x :: RPar :: s) = (Haskell.Var x, s)
    | parse (Var x :: s) = (Haskell.Var x, s)
    | parse (LPar :: Char c :: RPar :: s) =
        (Haskell.Literal (Haskell.Char c), s)
    | parse (Char c :: s) =
        (Haskell.Literal (Haskell.Char c), s)
    | parse (LPar :: Int i :: RPar :: s) =
        (Haskell.Literal (Haskell.Int i), s)
    | parse (Int i :: s) =
        (Haskell.Literal (Haskell.Int i), s)
    | parse (LPar :: Real r :: RPar :: s) =
        (Haskell.Literal (Haskell.Real r), s)
    | parse (Real r :: s) =
        (Haskell.Literal (Haskell.Real r), s)
    | parse (LPar :: Bool b :: RPar :: s) =
        (Haskell.Literal (Haskell.Bool b), s)
    | parse (Bool b :: s) =
        (Haskell.Literal (Haskell.Bool b), s)
    | parse (LPar :: RPar :: s) = (Haskell.Literal Haskell.Unit, RPar :: s)
    | parse (LPar :: s) =
        (case parse s of
           (l, And :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.And (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " && ...'"))
         | (l, Or :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Or (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " || ...'"))
         | (l, Leq :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Leq (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " <= ...'"))
         | (l, Lt :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Lt (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " < ...'"))
         | (l, Geq :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Geq (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " >= ...'"))
         | (l, Gt :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Gt (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " > ...'"))
         | (l, EqOp :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Eq (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " == ...'"))
         | (l, Add :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Add (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " + ...'"))
         | (l, Sub :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Sub (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " - ...'"))
         | (l, Mul :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Mul (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " * ...'"))
         | (l, Div :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Div (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " / ...'"))
         | (l, Pow :: s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Op (Haskell.Pow (l, r)), s'')
              | _ =>
                  raise Fail
                    ("missing ')' after '(" ^ Haskell.toString l ^ " ^ ...'"))
         | (l, s') =>
             (case parse s' of
                (r, RPar :: s'') => (Haskell.Call (l, r), s'')
              | _ => raise Fail "missing ')' after '(fun arg'"))
    | parse (a :: _) =
        raise Fail (debug a)
    | parse [] = raise Fail "EOF"
end

(* | parse a :: s' = *)
(*     raise Fail "found " ^ debug a *)

(* fun tts (LPar :: s) = " LPar " ^ tts s *)
(*   | tts (RPar :: s) = " RPar " ^ tts s *)
(*   | tts (Arrow :: s) = " Arrow " ^ tts s *)
(*   | tts (Var v :: s) = *)
(*       " Var(" ^ v ^ ") " ^ tts s *)
(*   | tts (Char c :: s) = *)
(*       " Char(" ^ Char.toString c ^ ") " ^ tts s *)
(*   | tts (Int i :: s) = *)
(*       " Int(" ^ Int.toString i ^ ") " ^ tts s *)
(*   | tts (Real r :: s) = *)
(*       " Real(" ^ Real.toString r ^ ") " ^ tts s *)
(*   | tts (Bool b :: s) = *)
(*       " Bool(" ^ Bool.toString b ^ ") " ^ tts s *)
(*   | tts (And :: s) = " And " ^ tts s *)
(*   | tts (Or :: s) = " Or " ^ tts s *)
(*   | tts (Leq :: s) = " Leq " ^ tts s *)
(*   | tts (Lt :: s) = " Lt " ^ tts s *)
(*   | tts (Geq :: s) = " Geq " ^ tts s *)
(*   | tts (Gt :: s) = " Gt " ^ tts s *)
(*   | tts (EqOp :: s) = " EqOp " ^ tts s *)
(*   | tts (Add :: s) = " Add " ^ tts s *)
(*   | tts (Sub :: s) = " Sub " ^ tts s *)
(*   | tts (Mul :: s) = " Mul " ^ tts s *)
(*   | tts (Div :: s) = " Div " ^ tts s *)
(*   | tts (Pow :: s) = " Pow " ^ tts s *)
(*   | tts (Eq :: s) = " Eq " ^ tts s *)
(*   | tts (Backslash :: s) = " BackSlash " ^ tts s *)
(*   | tts (If :: s) = " If " ^ tts s *)
(*   | tts (Then :: s) = " Then " ^ tts s *)
(*   | tts (Else :: s) = " Else " ^ tts s *)
(*   | tts (Let :: s) = " Let " ^ tts s *)
(*   | tts (In :: s) = " In " ^ tts s *)
(*   | tts _ = "" *)
(**)
(**)


(* (case parse s'' of *)
(*    (belse, s''') => *)
(*      (Haskell.IfThenElse (cond, bif, belse), s''')) *)
(* | _ => raise Fail "") *)
(* (case parse s'' of *)
(*    (body, s''') => (Haskell.Let (x, exp, body), s''')) *)
val z = 0;
