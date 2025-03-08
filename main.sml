fun check s =
  (case s of
     [] => ()
   | _ =>
       let
         val (exp, s') = Parser.parse s
         val res = Haskell.eval (exp, [])
       in
         check s'
       end)

fun interpret s =
  (case s of
     [] => ()
   | _ =>
       let
         val (exp, s') = Parser.parse s
         val res = Haskell.eval (exp, [])
         val _ = TextIO.output (TextIO.stdErr, Haskell.debug res ^ "\n")
       in
         interpret s'
       end)

fun check_version (args :: l) =
      (String.compare (args, "--version") = EQUAL) orelse check_version l
  | check_version [] = false

fun filename (args :: _) = args
  | filename [] = "main.hsml"

fun args_join (arg :: l) = arg ^ " ------- " ^ args_join l
  | args_join [] = "";

val capabilities = JSON.OBJECT
  [ ("jsonrpc", JSON.STRING "2.0")
  , ("id", JSON.INT 1)
  , ( "result"
    , JSON.OBJECT
        [( "capabilities"
         , JSON.OBJECT
             [ ( "textDocumentSync"
               , JSON.OBJECT
                   [ ("openClose", JSON.BOOL true)
                   , ("change", JSON.INT 2)
                   , ("save", JSON.OBJECT [("includeText", JSON.BOOL true)])
                   ]
               )
             , ( "diagnosticProvider"
               , JSON.OBJECT
                   [ ("interFileDependencies", JSON.BOOL false)
                   , ("workspaceDiagnostics", JSON.BOOL false)
                   ]
               )
             , ( "publishDiagnostics"
               , JSON.OBJECT
                   [ ("relatedInformation", JSON.BOOL true)
                   , ( "tagSupport"
                     , JSON.OBJECT
                         [("valueSet", JSON.ARRAY [JSON.INT 1, JSON.INT 2])]
                     )
                   ]
               )
             ]
         )]
    )
  ]


val errors = JSON.OBJECT
  [ ("jsonrpc", JSON.STRING "2.0")
  , ("method", JSON.STRING "textDocument/publishDiagnostics")
  , ( "params"
    , JSON.OBJECT
        [ ("uri", JSON.STRING "file:///home/cicio/projects/haskell/main.hsml")
        , ( "diagnostics"
          , JSON.ARRAY
              [JSON.OBJECT
                 [ ( "range"
                   , JSON.OBJECT
                       [ ( "start"
                         , JSON.OBJECT
                             [("line", JSON.INT 0), ("character", JSON.INT 0)]
                         )
                       , ( "end"
                         , JSON.OBJECT
                             [("line", JSON.INT 0), ("character", JSON.INT 0)]
                         )
                       ]
                   )
                 , ("message", JSON.STRING "")
                 , ("severity", JSON.INT 1)
                 , ("source", JSON.STRING "hsml_ls")
                 ]]
          )
        ]
    )
  ]

val cleanup = JSON.OBJECT
  [ ("jsonrpc", JSON.STRING "2.0")
  , ("method", JSON.STRING "textDocument/publishDiagnostics")
  , ( "params"
    , JSON.OBJECT
        [ ("uri", JSON.STRING "file:///home/cicio/projects/haskell/main.hsml")
        , ( "diagnostics"
          , JSON.ARRAY
              [ (* JSON.OBJECT *) (*    [ ( "range" *) (*      , JSON.OBJECT *) (*          [ ( "start" *) (*            , JSON.OBJECT *) (*                [("line", JSON.INT 0), ("character", JSON.INT 0)] *) (*            ) *) (*          , ( "end" *) (*            , JSON.OBJECT *) (*                [("line", JSON.INT 0), ("character", JSON.INT 0)] *) (*            ) *) (*          ] *) (*      ) *) (*    , ("message", JSON.STRING "error message yay") *) (*    , ("severity", JSON.INT 1) *) (*    , ("source", JSON.STRING "hsml_ls") *) (*    ] *)]
          )
        ]
    )
  ]


val temp = TextIO.openOut "temp"
val _ = JSONPrinter.print (temp, capabilities)
val _ = TextIO.closeOut temp
val tempIn = TextIO.openIn "temp"
val capabilities = TextIO.inputAll tempIn
val _ = TextIO.closeIn tempIn

val temp = TextIO.openOut "temp"
val _ = JSONPrinter.print (temp, errors)
val _ = TextIO.closeOut temp
val tempIn = TextIO.openIn "temp"
val z2 = TextIO.inputAll tempIn
val _ = TextIO.closeIn tempIn

val temp = TextIO.openOut "temp"
val _ = JSONPrinter.print (temp, cleanup)
val _ = TextIO.closeOut temp
val tempIn = TextIO.openIn "temp"
val cleanupJson = TextIO.inputAll tempIn
val _ = TextIO.closeIn tempIn

(* val x = CharBuffer.new *)
(* val x = JSONBufferPrinter.print *)
(* val temp = TextIO.openOut "temp" *)
(* val x = CharVectorSlice.full "" *)
(* val _ = JSONUtil.get (x, obj); *)

(* JSONBufferPrinter.new *)

(* val buf = CharVectorSlice.full "" *)
(* val z = JSONStreamPrinter.new *)

(* CharBuffer.new *)
(* val tempIn = TextIO.openIn "temp" *)
(* val z = TextIO.inputAll tempIn; *)
(* val _ = print z *)
(* val _ = print (TextIO.inputAll (TextIO.openIn "temp")) *)
(* val _ = print (Int.toString (length (explode z))) *)

(* val x = TextIO.openOut "" *)
(* val y = JSONPrinter.print (x, obj) *)
(* val buf = Word8Buffer.new 0 *)
(* val ss = Substring.size *)
(* val z = TextIO.outputSubstr (x, Substring.full "") *)

(* { *)
(*   "jsonrpc": "2.0", *)
(*   "id": 1, *)
(*   "result": { *)
(*     // Result data goes here *)
(*   } *)
(* } *)

(* fun trimStart(a ^ "s") *)

fun trimStart (a :: s, f) =
      (* let *)
      (* val _ = () *)

      (* val _ = print ((Char.toString a) ^ "\n") *)
      (* in *)
      if a = #"\"" then "\"" ^ trimStart (s, f)
      else if Char.isSpace a then trimStart (s, f)
      else if f then Char.toString a ^ trimStart (s, true)
      else if a = #"{" then "{" ^ trimStart (s, true)
      else trimStart (s, false)
  (* end *)
  | trimStart ([], _) = ""

(* val _ = TextIO.output (logs, request) *)

fun clean _ =
  let
    val _ = print
      ("Content-Length: " ^ Int.toString (String.size cleanupJson) ^ "\r\n\r\n"
       ^ cleanupJson)
  in
    ()
  end

fun fail (e, uri) =
  let
    val errors = JSON.OBJECT
      [ ("jsonrpc", JSON.STRING "2.0")
      , ("method", JSON.STRING "textDocument/publishDiagnostics")
      , ( "params"
        , JSON.OBJECT
            [ ("uri", JSON.STRING uri)
            , ( "diagnostics"
              , JSON.ARRAY
                  [JSON.OBJECT
                     [ ( "range"
                       , JSON.OBJECT
                           [ ( "start"
                             , JSON.OBJECT
                                 [ ("line", JSON.INT 0)
                                 , ("character", JSON.INT 0)
                                 ]
                             )
                           , ( "end"
                             , JSON.OBJECT
                                 [ ("line", JSON.INT 0)
                                 , ("character", JSON.INT 0)
                                 ]
                             )
                           ]
                       )
                     , ("message", JSON.STRING e)
                     , ("severity", JSON.INT 1)
                     , ("source", JSON.STRING "hsml_ls")
                     ]]
              )
            ]
        )
      ]
    val temp = TextIO.openOut "temp"
    val _ = JSONPrinter.print (temp, errors)
    val _ = TextIO.closeOut temp
    val tempIn = TextIO.openIn "temp"
    val response = TextIO.inputAll tempIn
    val _ = TextIO.closeIn tempIn
    val _ = print
      ("Content-Length: " ^ Int.toString (String.size response) ^ "\r\n\r\n"
       ^ response)
  in
    ()
  end

fun cleanup (a :: s) =
      if a = #"\\" then cleanup s else Char.toString a ^ cleanup s
  | cleanup [] = ""

fun inf _ =
  let
    val request = TextIO.input TextIO.stdIn
    val request = trimStart (explode request, false)
    val json = JSONParser.parse (JSONParser.openString request)
    val _ =
      (case (JSONUtil.findField json) "params" of
         SOME params =>
           (case (JSONUtil.findField params) "textDocument" of
              SOME textDocument =>
                (case (JSONUtil.findField textDocument) "text" of
                   SOME text =>
                     let
                       val code = JSONUtil.asString text
                       val _ =
                         let val _ = check (Parser.tokenize (explode code))
                         in ()
                         end
                         handle Fail e =>
                           fail
                             ( e
                             , "file:///home/cicio/projects/haskell/main.hsml"
                             )
                     in
                       ()
                     end
                 | _ =>
                     (case (JSONUtil.findField textDocument) "uri" of
                        SOME uri =>
                          let
                            val uri = JSONUtil.asString uri
                            val uri = String.extract (uri, 9, NONE)
                            val uri = cleanup (explode uri)
                            val _ = TextIO.output (TextIO.stdErr, uri)
                            val inputStream = TextIO.openIn uri
                            val code = TextIO.inputAll inputStream
                            val _ = TextIO.output (TextIO.stdErr, code)
                            val _ =
                              let val _ = check (Parser.tokenize (explode code))
                              in ()
                              end
                              handle Fail e => fail (e, "file://" ^ uri)
                          in
                            ()
                          end
                      | _ => clean 1))
            | _ =>
                (case (JSONUtil.findField params) "text" of
                   SOME text =>
                     let
                       val code = JSONUtil.asString text
                       val _ =
                         let val _ = check (Parser.tokenize (explode code))
                         in ()
                         end
                         handle Fail e =>
                           fail
                             ( e
                             , "file:///home/cicio/projects/haskell/main.hsml"
                             )

                     in
                       ()
                     end
                 | _ => clean 1))
       | _ => clean 1)
  in
    inf 1
  end

(* val _ = print *)
(*   ("Content-Length: " ^ Int.toString (String.size z2) ^ "\r\n\r\n" ^ z2) *)

structure Main =
struct
  fun main _ =
    let
      val args = CommandLine.arguments ()
      val _ =
        if check_version args then
          print "42.0"
        else if (length args) = 1 then
          let
            val file = filename args
            val inputStream = TextIO.openIn file
            val code = TextIO.inputAll inputStream
            val _ = interpret (Parser.tokenize (explode code))
          in
            ()
          end
        else
          let
            val request = TextIO.input TextIO.stdIn

            val logs = TextIO.openOut "logs"
            val _ = TextIO.output (logs, request)
            val request = trimStart (explode request, false)
            val json = JSONParser.parse (JSONParser.openString request)
            val _ =
              if (JSONUtil.hasField "id") json then
                print
                  ("Content-Length: " ^ Int.toString (String.size capabilities)
                   ^ "\r\n\r\n" ^ capabilities)
              else
                ()
            val _ = inf 1
          in
            ()
          end
    in
      OS.Process.success
    end
    handle Fail e =>
      let val _ = TextIO.output (TextIO.stdErr, "Fail: " ^ e ^ "\n")
      in OS.Process.success
      end
end

(* val _ = TextIO.output (TextIO.stdErr, "error") *)
(* if (JSONUtil.hasField "params") json then *)
(* let *)
(*   val params = (JSONUtil.lookupField json) "params" *)
(**)
(*   val textDocument = (JSONUtil.lookupField params) "textDocument" *)
(* val text = (JSONUtil.lookupField textDocument) "text" *)
(* val code = JSONUtil.asString text *)
(* val _ = *)
(*   let val _ = interpret (Parser.tokenize (explode code)) *)
(*   in () *)
(*   end *)
(*   handle Fail e => *)
(*     let *)
(*       val errors = JSON.OBJECT *)
(*         [ ("jsonrpc", JSON.STRING "2.0") *)
(*         , ("method", JSON.STRING "textDocument/publishDiagnostics") *)
(*         , ( "params" *)
(*           , JSON.OBJECT *)
(*               [ ( "uri" *)
(*                 , JSON.STRING *)
(*                     "file:///home/cicio/projects/haskell/main.hsml" *)
(*                 ) *)
(*               , ( "diagnostics" *)
(*                 , JSON.ARRAY *)
(*                     [JSON.OBJECT *)
(*                        [ ( "range" *)
(*                          , JSON.OBJECT *)
(*                              [ ( "start" *)
(*                                , JSON.OBJECT *)
(*                                    [ ("line", JSON.INT 0) *)
(*                                    , ("character", JSON.INT 0) *)
(*                                    ] *)
(*                                ) *)
(*                              , ( "end" *)
(*                                , JSON.OBJECT *)
(*                                    [ ("line", JSON.INT 0) *)
(*                                    , ("character", JSON.INT 0) *)
(*                                    ] *)
(*                                ) *)
(*                              ] *)
(*                          ) *)
(*                        , ("message", JSON.STRING e) *)
(*                        , ("severity", JSON.INT 1) *)
(*                        , ("source", JSON.STRING "hsml_ls") *)
(*                        ]] *)
(*                 ) *)
(*               ] *)
(*           ) *)
(*         ] *)
(*       val temp = TextIO.openOut "temp" *)
(*       val _ = JSONPrinter.print (temp, errors) *)
(*       val _ = TextIO.closeOut temp *)
(*       val tempIn = TextIO.openIn "temp" *)
(*       val response = TextIO.inputAll tempIn *)
(*       val _ = TextIO.closeIn tempIn *)
(*       val _ = print *)
(*         ("Content-Length: " ^ Int.toString (String.size response) *)
(*          ^ "\r\n\r\n" ^ response) *)
(*     in *)
(*       () *)
(*     end *)
(*   in *)
(*     () *)
(*   end *)
(* else *)
(*   () *)

(* val _ = TextIO.output (TextIO.stdErr, "-") *)
(* val _ = print *)
(*   ("Content-Length: " ^ Int.toString (String.size z) ^ "\r\n\r\n" *)
(*    ^ z) *)


(* val _ = TextIO.output (TextIO.stdErr, request) *)
(* val _ = TextIO.output (TextIO.stdErr, "n1gga") *)

(* val _ = print request *)
(* val json = (TextIO.stdOut , JSONParser.parse (JSONParser.openStream TextIO.stdIn)) *)
(* val json = JSONParser.parse (JSONParser.openStream TextIO.stdIn) *)
(* val _ = print *)
(*   ("Content-Length: " ^ Int.toString (String.size z) ^ "\r\n\r\n" *)
(*    ^ z) *)


(* val _ = TextIO.output (logs, request) *)
(* val request = trimStart (explode request, false) *)
(* val json = JSONParser.parse (JSONParser.openString request) *)
(* val _ = print *)
(*   ("Content-Length: " ^ Int.toString (String.size z2) ^ "\r\n\r\n" *)
(*    ^ z2) *)


(*let val _ = JSONPrinter.print (TextIO.stdOut, obj) *)

(* val zero = fn x => (fn y => y) *)
(* val one = fn x => (fn y => x y); *)
(* val two = fn x => (fn y => x (x (y))) *)
(* val three = fn x => (fn y => x (x (x (y)))) *)
(* val _ = TextIO.output (TextIO.stdErr, request) *)
(* val _ = TextIO.output (TextIO.stdErr, args_join args) *)
(* val inputStream = TextIO.openIn "main.hsml" *)
(* val content = TextIO.inputAll inputStream *)
(* val _ = TextIO.closeIn inputStream *)
(* val (exp, _) = Parser.parse (Parser.tokenize (explode content)) *)
(* val _ = print (Haskell.debug (Haskell.eval (exp, [])) ^ "\n") *)

(* val _ = print (Parser.tts (Parser.tokenize (explode content)) ^ "\n\n") *)
(* val _ = print (Haskell.toString (Haskell.Fun ("prova", "y", Haskell.Var "y"))) *)
(* val _ = print (Haskell.toString exp) *)
(* val _ = interpret (Parser.tokenize (explode content)) *)

(* val obj = JSON.OBJECT *)
(*   [ ("a", JSON.INT 23) *)
(*   , ("b", JSON.ARRAY [JSON.BOOL false, JSON.BOOL true]) *)
(*   , ("c", JSON.STRING "hello world") *)
(*   ]; *)

(* val z = *)
(*   "{\"jsonrpc\": \"2.0\",\"id\": 1, \"result\": { \"isIncomplete\": false, \"items\": [ { \"label\": \"print\", \"kind\": 3, // Function \"detail\": \"print(value: any): void\", \"documentation\": \"Prints a value to the console.\" }, { \"label\": \"println\", \"kind\": 3, // Function \"detail\": \"println(value: any): void\", \"documentation\": \"Prints a value to the console and adds a new line.\" } ] } }" *)
(* let *)
(*   val _ = JSONPrinter.print *)
(*     (TextIO.stdOut, JSONParser.parse (JSONParser.openStream TextIO.stdIn)) *)
(* in *)
(*   OS.Process.success *)
(* end *)
(* { *)
(*     "jsonrpc": "2.0", *)
(*     "method": "textDocument/publishDiagnostics", *)
(*     "params": { *)
(*         "uri": "file://path/to/file.txt", *)
(*         "diagnostics": [ *)
(*             { *)
(*                 "range": { *)
(*                     "start": { *)
(*                         "line": 0, *)
(*                         "character": 0 *)
(*                     }, *)
(*                     "end": { *)
(*                         "line": 0, *)
(*                         "character": 1 *)
(*                     } *)
(*                 }, *)
(*                 "message": "Your error message here", *)
(*                 "severity": 1, *)
(*                 "source": "your-language-server" *)
(*             } *)
(*         ] *)
(*     } *)
(* } *)
(* val err = JSON.OBJECT *)
(*   [( "jsonrpc" *)
(*    , JSON.STRING "2.0" *)
(* ) (* , ( "error" *) (*   , JSON.OBJECT *) (*       [ ("code", JSON.INT 1) *) (*       , ("message", JSON.STRING "Syntax error at line 1") *) (*       , ( "data" *) (*         , JSON.OBJECT *) (*             [ ( "range" *) (*               , JSON.OBJECT *) (*                   [ ( "start" *) (*                     , JSON.OBJECT *) (*                         [("line", JSON.INT 0), ("character", JSON.INT 0)] *) (*                     ) *) (*                   , ( "end" *) (*                     , JSON.OBJECT *) (*                         [("line", JSON.INT 0), ("character", JSON.INT 10)] *) (*                     ) *) (*                   ] *) (*               ) *) (*             , ("severity", JSON.INT 1) *) (*             , ("source", JSON.STRING "hsml_ls") *) (*             , ("message", JSON.STRING "unexpected token }") *) (*             ] *) (*         ) *) (*       ] *) (*   ) *)] *)
(* [( "capabilities" *)
(*  , JSON.OBJECT *)
(*      [( "textDocumentSync" *)
(*       , JSON.OBJECT *)
(*           [ ("openClose", JSON.BOOL true) *)
(*           , ("change", JSON.INT 2) *)
(*           , ("save", JSON.OBJECT [("includeText", JSON.BOOL true)]) *)
(*           ] *)
(*       )] *)
(*  )] *)
(* { *)
(*     "jsonrpc": "2.0", *)
(*     "error": { *)
(*         "code": 1, *)
(*         "message": "Syntax error at line 1", *)
(*         "data": { *)
(*             "range": { *)
(*                 "start": { *)
(*                     "line": 0, *)
(*                     "character": 0 *)
(*                 }, *)
(*                 "end": { *)
(*                     "line": 0, *)
(*                     "character": 10 *)
(*                 } *)
(*             }, *)
(*             "severity": 1, *)
(*             "source": "compiler", *)
(*             "message": "Unexpected token '}'" *)
(*         } *)
(*     } *)
(* } *)
(* "textDocumentSync": { *)
(*               "openClose": true, *)
(*               "change": 2, *)
(*               "save": { "includeText": true } *)
(*           }, *)
(* val x = CharBuffer.new *)
(* val z = JSONBufferPrinter.print (x, obj) *)
(* val z = JSONBufferPrinter.print (x, obj) *)

val x = 0
