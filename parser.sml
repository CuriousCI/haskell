(* multiple stacks *)

structure Parser =
struct
  (* datatype Lex = *)
  (* Empty *)
  (*   Word of string *)
  (* | Unknown of string *)
  (* | Var of string *)
  (* | Fn of string *)
  (* | Literal of Haskell.Literal *)
  (* | If *)
  (* | Let *)

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
  | Plus
  | Minus
  | Times
  | Div
  | Pow

  | Eq
  | BackSlash

  | If
  | Then
  | Else
  (* | Let *)

  fun tts (LPar :: s) = " LPar " ^ tts s
    | tts (RPar :: s) = " RPar " ^ tts s
    | tts (Arrow :: s) = " Arrow " ^ tts s
    | tts (Var v :: s) =
        " Var(" ^ v ^ ") " ^ tts s
    | tts (Char c :: s) =
        " Char(" ^ Char.toString c ^ ") " ^ tts s
    | tts (Int i :: s) =
        " Int(" ^ Int.toString i ^ ") " ^ tts s
    | tts (Real r :: s) =
        " Real(" ^ Real.toString r ^ ") " ^ tts s
    | tts (Bool _ :: s) = " Bool " ^ tts s
    | tts (And :: s) = " And " ^ tts s
    | tts (Or :: s) = " Or " ^ tts s
    | tts (Leq :: s) = " Leq " ^ tts s
    | tts (Lt :: s) = " Lt " ^ tts s
    | tts (Geq :: s) = " Geq " ^ tts s
    | tts (Gt :: s) = " Gt " ^ tts s
    | tts (EqOp :: s) = " EqOp " ^ tts s
    | tts (Plus :: s) = " Plus " ^ tts s
    | tts (Minus :: s) = " Minus " ^ tts s
    | tts (Times :: s) = " Times " ^ tts s
    | tts (Div :: s) = " Div " ^ tts s
    | tts (Pow :: s) = " Pow " ^ tts s
    | tts (Eq :: s) = " Eq " ^ tts s
    | tts (BackSlash :: s) = " BackSlash " ^ tts s
    | tts (If :: s) = " If " ^ tts s
    | tts (Then :: s) = " Then " ^ tts s
    | tts (Else :: s) = " Else " ^ tts s
    | tts _ = ""


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

  fun tokenize [] = []
    | tokenize (#"(" :: s) = LPar :: tokenize s
    | tokenize (#")" :: s) = RPar :: tokenize s
    | tokenize (#"-" :: #">" :: s) = Arrow :: tokenize s
    | tokenize (#"&" :: #"&" :: s) = And :: tokenize s
    | tokenize (#"|" :: #"|" :: s) = Or :: tokenize s
    | tokenize (#"<" :: #"=" :: s) = Leq :: tokenize s
    | tokenize (#"<" :: s) = Lt :: tokenize s
    | tokenize (#">" :: #"=" :: s) = Geq :: tokenize s
    | tokenize (#">" :: s) = Gt :: tokenize s
    | tokenize (#"=" :: #"=" :: s) = EqOp :: tokenize s
    | tokenize (#"+" :: s) = Plus :: tokenize s
    | tokenize (#"-" :: s) = Minus :: tokenize s
    | tokenize (#"*" :: s) = Times :: tokenize s
    | tokenize (#"/" :: s) = Div :: tokenize s
    | tokenize (#"^" :: s) = Pow :: tokenize s
    | tokenize (#"=" :: s) = Eq :: tokenize s
    | tokenize (#"\\" :: s) = BackSlash :: tokenize s
    | tokenize (#"T" :: #"r" :: #"u" :: #"e" :: s) = Bool true :: tokenize s
    | tokenize (#"F" :: #"a" :: #"l" :: #"s" :: #"e" :: s) =
        Bool false :: tokenize s
    | tokenize (#"'" :: c :: #"'" :: s) = Char c :: tokenize s
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
                     | NONE => raise Fail "")
                | RealSeq r =>
                    (case Real.fromString r of
                       SOME r => Real r :: tokenize cs
                     | NONE => raise Fail ""))
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
                        | NONE => raise Fail "")
                   | RealSeq r =>
                       (case Real.fromString r of
                          SOME r => Real r :: tokenize cs
                        | NONE => raise Fail ""))
                end
              else
                raise Fail ""))

  fun parse (LPar :: Var name :: Var x :: Eq :: LPar :: ts) = 0
    | parse (LPar :: Var x :: RPar :: ts) = 0
    | parse (LPar :: BackSlash :: Var x :: Arrow :: LPar :: ts) = 0
    | parse (LPar :: Var x :: Plus :: ts) = 0
    | parse _ = 0

end
