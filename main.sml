val obj = JSON.OBJECT
  [ ("a", JSON.INT 23)
  , ("b", JSON.ARRAY [JSON.BOOL false, JSON.BOOL true])
  , ("c", JSON.STRING "hello world")
  ];

(*let val _ = JSONPrinter.print (TextIO.stdOut, obj) *)

structure Main =
struct
  fun main _ =
    let
      val inputStream = TextIO.openIn "main.hsml"
      val content = TextIO.inputAll inputStream
      val _ = TextIO.closeIn inputStream
      val _ = print (Parser.tts (Parser.tokenize (explode content)))
    (* val _ = print (Parser.toString (Parser.parse (explode content, []))) *)
    in
      OS.Process.success
    end
end (* let *) (*   val _ = JSONPrinter.print *) (*     (TextIO.stdOut, JSONParser.parse (JSONParser.openStream TextIO.stdIn)) *) (* in *) (*   OS.Process.success *) (* end *)
